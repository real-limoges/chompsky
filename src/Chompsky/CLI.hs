{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | CLI entry point.
module Chompsky.CLI (run, withErrorBoundary) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically, newTChanIO, writeTChan)
import Control.Exception (Handler (..), IOException, catches, displayException)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)
import HsLua.Core qualified as Lua
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Chompsky.Config (AppConfig (..), loadConfig)
import Chompsky.IO.Csv (buildCsvRows, writeExtractions, writeReviews)
import Chompsky.IO.CsvInput (readInputCsv, readInputCsvLimit)
import Chompsky.IO.Workers (processStreaming)
import Chompsky.Pipeline (ProcessResult (..), processRemarkPure)
import Chompsky.TUI (runTUI, withLineBuffering)
import Chompsky.Types

run :: IO ()
run = do
    (globalOpts, cmd) <- execParser opts
    let dir = configDir globalOpts
    cfg <-
        withErrorBoundary "loading config" $ loadConfig dir
    case cmd of
        ParseOne -> runParseOne cfg
        Backfill bo -> withLineBuffering $ runBackfill cfg bo
        DryRun dro -> runDryRun cfg dro

-- | Catch known exceptions, print a labelled message to stderr, and exit.
withErrorBoundary :: String -> IO a -> IO a
withErrorBoundary label act =
    act
        `catches` [ Handler $ \(e :: ChompskyError) -> die (displayException e)
                  , Handler $ \(e :: IOException) -> die (displayException e)
                  , Handler $ \(e :: Lua.Exception) -> die (displayException e)
                  ]
    where
        die msg = do
            hPutStrLn stderr $ "Error " <> label <> ": " <> msg
            exitFailure

runBackfill :: AppConfig -> BackfillOpts -> IO ()
runBackfill cfg bo = do
    chan <- newTChanIO
    withAsync (workerThread chan) $ \_ ->
        runTUI chan
    where
        send chan ev = atomically (writeTChan chan ev)
        workerThread chan = do
            send chan InputFetchStarted
            rows <- withErrorBoundary "reading input CSV" $ readInputCsv (bfInput bo)
            send chan (InputFetchDone (length rows))
            results <- processStreaming cfg (`mapM_` rows) $ \_ result ->
                send chan (ItemProcessed (needsLlm (prExtraction result)) (parserConfidence (prExtraction result)))
            now <- getCurrentTime
            let nowText = T.pack (show now)
                (extractionRows, reviewRows) = buildCsvRows nowText results
            send chan CsvWriteStarted
            withErrorBoundary "writing extractions CSV" $ writeExtractions (bfOutput bo) extractionRows
            withErrorBoundary "writing reviews CSV" $
                if null reviewRows
                    then pure ()
                    else writeReviews (bfReviews bo) reviewRows
            send chan CsvWriteDone
            send chan BackfillFinished

runDryRun :: AppConfig -> DryRunOpts -> IO ()
runDryRun cfg dro = do
    rows <-
        withErrorBoundary "reading input CSV" $
            readInputCsvLimit (drInput dro) (drLimit dro)
    results <- processStreaming cfg (`mapM_` rows) (\_ _ -> pure ())
    mapM_
        ( \(row, ProcessResult ext _) -> do
            TIO.putStrLn $ "--- " <> rowId row <> " ---"
            BL.putStrLn $ Aeson.encode ext
            TIO.putStrLn ""
        )
        results

runParseOne :: AppConfig -> IO ()
runParseOne cfg = do
    remark <- TIO.getContents
    let ProcessResult ext _ = processRemarkPure cfg remark
    BL.putStrLn $ Aeson.encode ext

newtype GlobalOpts = GlobalOpts
    { configDir :: FilePath
    }

data Command
    = Backfill BackfillOpts
    | DryRun DryRunOpts
    | ParseOne

data BackfillOpts = BackfillOpts
    { bfInput :: FilePath
    , bfOutput :: FilePath
    , bfReviews :: FilePath
    }

data DryRunOpts = DryRunOpts
    { drInput :: FilePath
    , drLimit :: Int
    }

globalOptsParser :: Parser GlobalOpts
globalOptsParser =
    GlobalOpts
        <$> strOption
            ( long "config-dir"
                <> metavar "DIR"
                <> value "config"
                <> showDefault
                <> help "Directory containing configuration files"
            )

commandParser :: Parser Command
commandParser =
    subparser
        ( command "backfill" (info (Backfill <$> backfillParser) (progDesc "Process all rows from input CSV"))
            <> command "dry-run" (info (DryRun <$> dryRunParser) (progDesc "Parse and print, no output files"))
            <> command "parse-one" (info (pure ParseOne) (progDesc "Parse single remark from stdin"))
        )

backfillParser :: Parser BackfillOpts
backfillParser =
    BackfillOpts
        <$> strOption (long "input" <> short 'i' <> metavar "FILE" <> help "Input CSV file")
        <*> strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Extractions output CSV")
        <*> strOption (long "reviews" <> short 'r' <> metavar "FILE" <> help "Reviews output CSV")

dryRunParser :: Parser DryRunOpts
dryRunParser =
    DryRunOpts
        <$> strOption (long "input" <> short 'i' <> metavar "FILE" <> help "Input CSV file")
        <*> option auto (long "limit" <> value 100 <> showDefault <> help "Max rows to process")

opts :: ParserInfo (GlobalOpts, Command)
opts =
    info
        (((,) <$> globalOptsParser <*> commandParser) <**> helper)
        (fullDesc <> progDesc "Domain-agnostic text extraction pipeline" <> header "chompsky")
