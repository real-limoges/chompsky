{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | CLI entry point.
module Chompsky.CLI
    ( run
    , withErrorBoundary
    , BackfillOpts (..)
    , backfillWorker
    ) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM (atomically, newTChanIO, writeTChan)
import Control.Exception
    ( Handler (..)
    , IOException
    , SomeException
    , catch
    , catches
    , displayException
    , throwIO
    )
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import HsLua.Core qualified as Lua
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Chompsky.Config (AppConfig (..), loadConfig)
import Chompsky.IO.Csv (toExtractionRow, toReviewRow, withCsvOutput)
import Chompsky.IO.CsvInput (countInputRows, readInputCsvLimit, streamInputCsv)
import Chompsky.IO.Workers (processStreaming)
import Chompsky.Pipeline (ProcessResult (..), processRemarkPure)
import Chompsky.TUI (formatSummary, runTUI, withLineBuffering)
import Chompsky.TUI.State (TUIState (..))
import Chompsky.Types

run :: IO ()
run = do
    (globalOpts, cmd) <- execParser opts
    let dir = configDir globalOpts
    cfg <-
        withErrorBoundary "loading config" $ loadConfig dir
    case cmd of
        ParseOne -> runParseOne cfg
        Backfill bo -> withLineBuffering $ withErrorBoundary "running backfill" $ runBackfill cfg bo
        DryRun dro -> runDryRun cfg dro

-- | Catch known exceptions, print a labeled message to stderr, and exit.
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

{- | Run the worker beside the TUI. A worker failure is reported to the TUI
(so it halts instead of waiting forever) and then re-raised here, so the
outer 'withErrorBoundary' prints it and the process exits non-zero. Quitting
the TUI before the worker finishes cancels the worker and also exits non-zero.
-}
runBackfill :: AppConfig -> BackfillOpts -> IO ()
runBackfill cfg bo = do
    chan <- newTChanIO
    let send ev = atomically (writeTChan chan ev)
        reportingFailure act =
            act `catch` \(e :: SomeException) -> do
                send (BackfillFailed (T.pack (displayException e)))
                throwIO e
    withAsync (reportingFailure (backfillWorker cfg bo send)) $ \worker -> do
        finalSt <- runTUI chan
        if tsFinished finalSt
            then do
                wait worker
                putStrLn (formatSummary finalSt)
            else do
                hPutStrLn stderr ("Aborted after " <> show (tsProcessed finalSt) <> " rows; output files left unchanged.")
                exitFailure

{- | Count the input, then stream it: each row is read, processed and written
before the next is read, so memory stays flat however large the input is.
Reports progress through @send@ and throws on any failure; the caller turns
that into 'BackfillFailed'. Both output files appear only when the run
completes (see 'withCsvOutput'), and the reviews file is always written,
header only when nothing was flagged, so a stale one is never left behind.
-}
backfillWorker :: AppConfig -> BackfillOpts -> (BackfillEvent -> IO ()) -> IO ()
backfillWorker cfg bo send = do
    send InputFetchStarted
    total <- countInputRows (bfInput bo)
    send (InputFetchDone total)
    withCsvOutput (bfOutput bo) $ \writeExtraction ->
        withCsvOutput (bfReviews bo) $ \writeReview ->
            processStreaming cfg (streamInputCsv (bfInput bo)) $ \row result -> do
                parsedAt <- T.pack . iso8601Show <$> getCurrentTime
                writeExtraction (toExtractionRow parsedAt row result)
                mapM_ writeReview (toReviewRow row result)
                send (ItemProcessed (needsLlm (prExtraction result)) (parserConfidence (prExtraction result)))
    send BackfillFinished

runDryRun :: AppConfig -> DryRunOpts -> IO ()
runDryRun cfg dro = do
    rows <-
        withErrorBoundary "reading input CSV" $
            readInputCsvLimit (drInput dro) (drLimit dro)
    processStreaming cfg (`mapM_` rows) $ \row (ProcessResult ext _) -> do
        TIO.putStrLn $ "--- " <> rowId row <> " ---"
        BL.putStrLn $ Aeson.encode ext
        TIO.putStrLn ""

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
