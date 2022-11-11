{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.NFT where

import Contracts.NFT
import Plutus.Trace.Emulator qualified as Trace
import Wallet.Emulator.Wallet
import Control.Monad (void)

nftTrace :: Trace.EmulatorTrace ()
nftTrace = do
    let tn = "ABC"
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- Trace.activateContractWallet w1 endpoints
    h2 <- Trace.activateContractWallet w2 endpoints
    Trace.callEndpoint @"mint" h1 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w1
        }
    Trace.callEndpoint @"mint" h2 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w2
        }
    void $ Trace.waitNSlots 1
  
