{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Contracts.NFT where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

-- KPLC These imports were added to allow for the UPLC code generation of
-- KPLC the contract's minting policies.
import UntypedPlutusCore
import PlutusCore.Quote
import PlutusCore.Pretty
import           Plutus.V1.Ledger.Scripts  (mkMintingPolicyScript)
import Plutus.Script.Utils.V1.Scripts (scriptCurrencySymbol)
-- KPLC

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile
       [|| \oref' tn' -> Scripts.mkUntypedMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

-- KPLC These variables were added to allow for the UPLC code generation of
-- KPLC the contract's minting policies.
compiledNFTPolicy :: (PlutusTx.CompiledCode
                        (TxOutRef -> TokenName -> Scripts.UntypedMintingPolicy))
compiledNFTPolicy = $$(PlutusTx.compile
  [|| \oref' tn' -> Scripts.mkUntypedMintingPolicy $ mkPolicy oref' tn' ||])

-- uplcNFTPolicy :: String
-- uplcNFTPolicy =
--   displayPlcDebug $ PlutusTx.getPlc compiledNFTPolicy
--
uplcNFTPolicy :: String
uplcNFTPolicy =
  either display displayPlcDebug $ unDeBruijnProgram $ PlutusTx.getPlc compiledNFTPolicy
  where
    unDeBruijnProgram (Program ann ver term) = Program ann ver <$> (runQuoteT $ unDeBruijnTerm @(QuoteT (Either FreeVariableError)) $ term)

-- pirNFTPolicy :: String
pirNFTPolicy =
    PlutusCore.Pretty.prettyClassicDebug $ PlutusTx.getPir compiledNFTPolicy
-- KPLC

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn

data NFTParams = NFTParams
    { npToken   :: !TokenName
    , npAddress :: !Address
    } deriving (Generic, FromJSON, ToJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams

mint :: NFTParams -> Contract w NFTSchema Text ()
mint np = do
    utxos <- utxosAt $ npAddress np
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tn      = npToken np
            let val     = Value.singleton (curSymbol oref tn) tn 1
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w1
        }
    callEndpoint @"mint" h2 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
