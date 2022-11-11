{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Main where

import Data.Default (Default (..))
import Data.Monoid (Sum (..))
import Ledger.Index (ValidatorMode (..))
import Options.Applicative
import Plutus.Contract.Wallet (ExportTx (..))
import Plutus.Trace (Command (..), ScriptsConfig (..), showStats, writeScriptsTo)
import Spec.Free qualified as Free
import Spec.NFT qualified as NFT
import Contracts.Free
import Contracts.NFT
import System.FilePath ((</>))

writeWhat :: Command -> String
writeWhat (Scripts FullyAppliedValidators) = "scripts (fully applied)"
writeWhat (Scripts UnappliedValidators)    = "scripts (unapplied)"

pathParser :: Parser FilePath
pathParser = strArgument (metavar "SCRIPT_PATH" <> help "output path")

commandParser :: Parser Command
commandParser = hsubparser $ scriptsParser

scriptsParser :: Mod CommandFields Command
scriptsParser =
    command "scripts" $
    info
        (Scripts <$> flag FullyAppliedValidators UnappliedValidators (long "unapplied-validators" <> short 'u' <> help "Write the unapplied validator scripts" <> showDefault))
        (fullDesc <> progDesc "Write fully applied validator scripts")

progParser :: ParserInfo ScriptsConfig
progParser =
    let p = ScriptsConfig <$> pathParser <*> commandParser
    in info
        (p <**> helper)
        (fullDesc
        <> progDesc "Run a number of emulator traces and write all validator scripts and/or partial transactions to SCRIPT_PATH"
        <> header "plutus-use-cases-scripts - extract validators and partial transactions from emulator traces"
        )

-- | `uncurry3` converts a curried function to a function on triples.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

writeScripts :: ScriptsConfig -> IO ()
writeScripts config = do
    -- Traces
    putStrLn $ "Writing " <> writeWhat (scCommand config) <> " to: " <> scPath config
    (Sum size, exBudget) <- foldMap (uncurry3 (writeScriptsTo config))
        [ ("free", Free.freeTrace, def),
          ("NFT", NFT.nftTrace, def) ]
    -- True policy                        
    putStrLn $ "Writing " <> (scPath config) </> "freeTruePolicy.uplc"
    writeFile ((scPath config) </> "freeTruePolicy.uplc") $
      Contracts.Free.uplcTruePolicy
    putStrLn $ "Writing " <> (scPath config) </> "freeTruePolicy.pir"
    writeFile ((scPath config) </> "freeTruePolicy.pir") $ show $
      Contracts.Free.pirTruePolicy
    -- False policy  
    putStrLn $ "Writing " <> (scPath config) </> "freeFalsePolicy.uplc"
    writeFile ((scPath config) </> "freeFalsePolicy.uplc") $
      Contracts.Free.uplcFalsePolicy
    putStrLn $ "Writing " <> (scPath config) </> "freeFalsePolicy.pir"
    writeFile ((scPath config) </> "freeFalsePolicy.pir") $ show $
      Contracts.Free.pirFalsePolicy
    -- NFT policy
    putStrLn $ "Writing " <> (scPath config) </> "nftPolicy.uplc"
    writeFile ((scPath config) </> "nftPolicy.uplc") $
      Contracts.NFT.uplcNFTPolicy
    putStrLn $ "Writing " <> (scPath config) </> "nftPolicy.pir"
    writeFile ((scPath config) </> "nftPolicy.pir") $ show $
      Contracts.NFT.pirNFTPolicy
    if size > 0 then
        putStrLn $ "Total " <> showStats size exBudget
    else pure ()

main :: IO ()
main = execParser progParser >>= writeScripts
