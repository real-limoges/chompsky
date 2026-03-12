{-# LANGUAGE OverloadedStrings #-}

-- | CSV input parsing for two-column (id, text) files.
module Chompsky.IO.CsvInput
    ( readInputCsv
    , readInputCsvLimit
    ) where

import Data.ByteString.Lazy qualified as BL
import Data.Csv (FromNamedRecord (..), (.:))
import Data.Csv qualified as Csv
import Data.Vector qualified as V

import Chompsky.Types (InputRow (..))

instance FromNamedRecord InputRow where
    parseNamedRecord r =
        InputRow
            <$> r .: "id"
            <*> r .: "text"

-- | Expects columns: @id@, @text@.
readInputCsv :: FilePath -> IO [InputRow]
readInputCsv path = do
    bs <- BL.readFile path
    case Csv.decodeByName bs of
        Left err -> fail $ "CSV parse error: " <> err
        Right (_, rows) -> pure (V.toList rows)

-- | Like 'readInputCsv' but returns at most @n@ rows.
readInputCsvLimit :: FilePath -> Int -> IO [InputRow]
readInputCsvLimit path n = take n <$> readInputCsv path
