{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | CSV output for extraction and review rows (cassava named-record API).
module Chompsky.IO.Csv
    ( ExtractionRow (..)
    , ReviewRow (..)
    , buildCsvRows
    , writeExtractions
    , writeReviews
    ) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL

import Data.Csv
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)

import Chompsky.Pipeline (ProcessResult (..))
import Chompsky.Types

import System.IO (IOMode (..), withFile)

-- | Split pipeline results into extraction rows (all) and review rows (flagged only).
buildCsvRows :: Text -> [(InputRow, ProcessResult)] -> ([ExtractionRow], [ReviewRow])
buildCsvRows nowText results = (extractionRows, reviewRows)
    where
        toJson :: Aeson.ToJSON a => a -> Text
        toJson = decodeUtf8 . BL.toStrict . Aeson.encode
        extractionRows =
            [ ExtractionRow
                { erRowId = rowId row
                , erExtractionJson = toJson (categories ext)
                , erNeedsLlm = needsLlm ext
                , erConfidence = confidenceToText (parserConfidence ext)
                , erConfidenceScore = confidenceScore ext
                , erParsedAt = nowText
                }
            | (row, ProcessResult ext _) <- results
            ]
        reviewRows = mapMaybe toReview results
        toReview (row, ProcessResult ext cleaned) =
            if needsLlm ext
                then
                    Just
                        ReviewRow
                            { rrRowId = rowId row
                            , rrRemarksClean = T.pack (show cleaned)
                            , rrTriageReason = fromMaybe "" (llmReason ext)
                            , rrTriageDetails = toJson (triageDetails ext)
                            , rrPartialExtraction = toJson (categories ext)
                            }
                else Nothing

-- | One row in the extractions output CSV.
data ExtractionRow = ExtractionRow
    { erRowId :: Text
    -- ^ Source row identifier.
    , erExtractionJson :: Text
    -- ^ Full categories map serialised as JSON.
    , erNeedsLlm :: Bool
    -- ^ Whether this row was flagged for LLM review.
    , erConfidence :: Text
    -- ^ Confidence tier as text (@"high"@, @"medium"@, @"low"@).
    , erConfidenceScore :: Double
    -- ^ Raw numeric confidence score.
    , erParsedAt :: Text
    -- ^ ISO-8601 timestamp of when the row was parsed.
    }

-- | One row in the reviews output CSV (only rows flagged for LLM review).
data ReviewRow = ReviewRow
    { rrRowId :: Text
    -- ^ Source row identifier.
    , rrRemarksClean :: Text
    -- ^ Cleaned remark text.
    , rrTriageReason :: Text
    -- ^ Human-readable triage reason.
    , rrTriageDetails :: Text
    -- ^ Triage details serialised as JSON.
    , rrPartialExtraction :: Text
    -- ^ Partial extraction serialised as JSON.
    }

instance ToNamedRecord ExtractionRow where
    toNamedRecord r =
        namedRecord
            [ "id" .= erRowId r
            , "extraction_json" .= erExtractionJson r
            , "needs_llm" .= (if erNeedsLlm r then "true" else "false" :: Text)
            , "confidence" .= erConfidence r
            , "confidence_score" .= (T.pack . show) (erConfidenceScore r)
            , "parsed_at" .= erParsedAt r
            ]

instance DefaultOrdered ExtractionRow where
    headerOrder _ =
        header
            [ "id"
            , "extraction_json"
            , "needs_llm"
            , "confidence"
            , "confidence_score"
            , "parsed_at"
            ]

instance ToNamedRecord ReviewRow where
    toNamedRecord r =
        namedRecord
            [ "id" .= rrRowId r
            , "remarks_clean" .= rrRemarksClean r
            , "triage_reason" .= rrTriageReason r
            , "triage_details" .= rrTriageDetails r
            , "partial_extraction" .= rrPartialExtraction r
            ]

instance DefaultOrdered ReviewRow where
    headerOrder _ =
        header
            [ "id"
            , "remarks_clean"
            , "triage_reason"
            , "triage_details"
            , "partial_extraction"
            ]

-- | Write extraction rows to a CSV file.
writeExtractions :: FilePath -> [ExtractionRow] -> IO ()
writeExtractions path rows =
    withFile path WriteMode $ \h ->
        BL.hPut h (encodeDefaultOrderedByName rows)

-- | Write review rows to a CSV file.
writeReviews :: FilePath -> [ReviewRow] -> IO ()
writeReviews path rows =
    withFile path WriteMode $ \h ->
        BL.hPut h (encodeDefaultOrderedByName rows)
