{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Main(main, ExportTx(..)) where

import qualified Cardano.Api            as C
import           Data.Default           (Default (..))
import           Data.Monoid            (Sum (..))
import           Ledger.Index           (ValidatorMode (..))
import           Options.Applicative
import           Plutus.Contract.Wallet (ExportTx (..))
import           Plutus.Trace           (Command (..), ScriptsConfig (..),
                                         showStats, writeScriptsTo)
import qualified Spec.Stablecoin        as Stablecoin
import           Contracts.Stablecoin 
import           System.FilePath        ((</>))

writeWhat :: Command -> String
writeWhat (Scripts FullyAppliedValidators) = "scripts (fully applied)"
writeWhat (Scripts UnappliedValidators)    = "scripts (unapplied)"
writeWhat Transactions{}                   = "transactions"
writeWhat MkTxLogs                         = "mkTx logs"

pathParser :: Parser FilePath
pathParser = strArgument (metavar "SCRIPT_PATH" <> help "output path")

protocolParamsParser :: Parser FilePath
protocolParamsParser = strOption (long "protocol-parameters" <> short 'p' <> help "Path to protocol parameters JSON file" <> showDefault <> value "protocol-parameters.json")

networkIdParser :: Parser C.NetworkId
networkIdParser =
    let p = C.Testnet . C.NetworkMagic <$> option auto (long "network-magic" <> short 'n' <> help "Cardano network magic. If none is specified, mainnet addresses are generated.")
    in p <|> pure C.Mainnet

commandParser :: Parser Command
commandParser = hsubparser $ mconcat [scriptsParser, transactionsParser, mkTxParser]

scriptsParser :: Mod CommandFields Command
scriptsParser =
    command "scripts" $
    info
        (Scripts <$> flag FullyAppliedValidators UnappliedValidators (long "unapplied-validators" <> short 'u' <> help "Write the unapplied validator scripts" <> showDefault))
        (fullDesc <> progDesc "Write fully applied validator scripts")

transactionsParser :: Mod CommandFields Command
transactionsParser =
    command "transactions" $
    info
        (Transactions <$> networkIdParser <*> protocolParamsParser)
        (fullDesc <> progDesc "Write partial transactions")

mkTxParser :: Mod CommandFields Command
mkTxParser =
    command "mktx" $
    info
        (pure MkTxLogs)
        (fullDesc <> progDesc "Write logs for 'mkTx' calls")

progParser :: ParserInfo ScriptsConfig
progParser =
    let p = ScriptsConfig <$> pathParser <*> commandParser
    in info
        (p <**> helper)
        (fullDesc
        <> progDesc "Run a number of emulator traces and write all validator scripts and/or partial transactions to SCRIPT_PATH"
        <> header "plutus-use-cases-scripts - extract validators and partial transactions from emulator traces"
        )

-- KPLC the following function was adapted from the original at
-- KPLC `plutus-apps/plutus-use-cases/scripts` to generate UPLC code from
-- KPLC the traces at `test/Spec/Stablecoin.hs` and the policy of the
-- KPLC Stablecoin contract.
writeScripts :: ScriptsConfig -> IO ()
writeScripts config = do
    putStrLn $ "Writing " <> writeWhat (scCommand config) <> " to: " <> scPath config
    (Sum size, exBudget) <- foldMap (uncurry3 (writeScriptsTo config))
        [ ("stablecoin_1", Stablecoin.stablecoinTrace, def)
        , ("stablecoin_2", Stablecoin.maxReservesExceededTrace, def)
        ]
    putStrLn $ "Writing " <> (scPath config) </> "stablecoinPolicy.uplc"
    writeFile ((scPath config) </> "stablecoinPolicy.uplc") $
      Contracts.Stablecoin.uplcStableCoinPolicy
    putStrLn $ "Writing " <> (scPath config) </> "stablecoinPolicy.pir"
    writeFile ((scPath config) </> "stablecoinPolicy.pir") $ show $
      Contracts.Stablecoin.pirStableCoinPolicy
    if size > 0 then
        putStrLn $ "Total " <> showStats size exBudget
    else pure ()
-- KPLC

-- | `uncurry3` converts a curried function to a function on triples.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

main :: IO ()
main = execParser progParser >>= writeScripts
