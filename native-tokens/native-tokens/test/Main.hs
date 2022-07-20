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

main :: IO ()
main = execParser progParser >>= writeScripts

writeScripts :: ScriptsConfig -> IO ()
writeScripts config = do
    putStrLn $ "Writing " <> writeWhat (scCommand config) <> " to: " <> scPath config
    (Sum size, exBudget) <- foldMap (uncurry3 (writeScriptsTo config))
        [ ("free", Free.freeTrace, def),
          ("NFT", NFT.nftTrace, def) ]
    if size > 0 then
        putStrLn $ "Total " <> showStats size exBudget
    else pure ()

-- | `uncurry3` converts a curried function to a function on triples.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
