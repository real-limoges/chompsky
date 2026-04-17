{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Per-stage NDJSON tracer. Reads remarks from stdin or a 2-column CSV and
emits one JSON object per line carrying snapshots from every pipeline stage.
-}
module Main (main) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative

import Chompsky.CLI (withErrorBoundary)
import Chompsky.Config (loadConfig)
import Chompsky.IO.CsvInput (readInputCsv)
import Chompsky.Pipeline (processRemarkTraced)
import Chompsky.Types (InputRow (..))

data Opts = Opts
    { optConfigDir :: FilePath
    , optInput :: Maybe FilePath
    }

main :: IO ()
main = do
    Opts {..} <- execParser optsInfo
    cfg <- withErrorBoundary "loading config" $ loadConfig optConfigDir
    remarks <- case optInput of
        Nothing -> stdinRemarks
        Just path -> map rowText <$> withErrorBoundary "reading input CSV" (readInputCsv path)
    mapM_ (BL.putStrLn . Aeson.encode . processRemarkTraced cfg) remarks

stdinRemarks :: IO [Text]
stdinRemarks = filter (not . T.null) . T.lines <$> TIO.getContents

optsParser :: Parser Opts
optsParser =
    Opts
        <$> strOption
            ( long "config-dir"
                <> metavar "DIR"
                <> value "config"
                <> showDefault
                <> help "Directory containing configuration files"
            )
        <*> optional
            ( strOption
                ( long "input"
                    <> short 'i'
                    <> metavar "FILE"
                    <> help "Input CSV (id,text); if omitted, reads remarks from stdin (one per line)"
                )
            )

optsInfo :: ParserInfo Opts
optsInfo =
    info
        (optsParser <**> helper)
        ( fullDesc
            <> progDesc "Emit per-stage pipeline traces as NDJSON"
            <> header "chompsky-trace"
        )
