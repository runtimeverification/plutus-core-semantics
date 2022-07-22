{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings   #-}

module Spec.Free where

import Contracts.Free
import Plutus.Trace.Emulator qualified as Trace
import Wallet.Emulator.Wallet
import Control.Monad (void)

freeTrace :: Trace.EmulatorTrace ()
freeTrace = do
    let tn = "ABC"
    h1 <- Trace.activateContractWallet (knownWallet 1) endpoints
    h2 <- Trace.activateContractWallet (knownWallet 2) endpoints
    Trace.callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    Trace.callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 444
        }
    _ <- Trace.waitNSlots 1
    Trace.callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    void $ Trace.waitNSlots 1
  
