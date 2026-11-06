{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | CSV output for extraction and review rows, written one row at a time.
module Chompsky.IO.Csv
    ( ExtractionRow (..)
    , ReviewRow (..)
    , toExtractionRow
    , toReviewRow
    , withCsvOutput
    ) where

import Control.Exception (onException)
import Data.Aeson qualified as Aeson
import Data.ByteString.Builder (hPutBuilder)
import Data.ByteString.Lazy qualified as BL
import Data.Csv
import Data.Csv.Builder (encodeDefaultOrderedNamedRecord, encodeHeader)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import System.Directory (removePathForcibly, renameFile)
import System.IO (IOMode (..), withBinaryFile)

import Chompsky.Pipeline (ProcessResult (..))
import Chompsky.Types

-- | One row in the extractions output CSV.
data ExtractionRow = ExtractionRow
    { erRowId :: Text
    , erExtractionJson :: Text
    -- ^ Full categories map serialized as JSON.
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
    , rrRemarksClean :: Text
    -- ^ Cleaned remark text.
    , rrTriageReason :: Text
    -- ^ Comma-joined tags of the triage rules that fired.
    , rrTriageDetails :: Text
    -- ^ Triage details serialized as JSON.
    , rrPartialExtraction :: Text
    -- ^ Partial extraction serialized as JSON.
    }

-- | The extractions row for one processed input row, stamped with @parsedAt@.
toExtractionRow :: Text -> InputRow -> ProcessResult -> ExtractionRow
toExtractionRow parsedAt row (ProcessResult ext _) =
    ExtractionRow
        { erRowId = rowId row
        , erExtractionJson = toJsonText (categories ext)
        , erNeedsLlm = needsLlm ext
        , erConfidence = confidenceToText (parserConfidence ext)
        , erConfidenceScore = confidenceScore ext
        , erParsedAt = parsedAt
        }

-- | The reviews row for one processed input row, if triage flagged it.
toReviewRow :: InputRow -> ProcessResult -> Maybe ReviewRow
toReviewRow row (ProcessResult ext cleaned)
    | needsLlm ext =
        Just
            ReviewRow
                { rrRowId = rowId row
                , rrRemarksClean = unCleanedText cleaned
                , rrTriageReason = fromMaybe "" (llmReason ext)
                , rrTriageDetails = toJsonText (triageDetails ext)
                , rrPartialExtraction = toJsonText (categories ext)
                }
    | otherwise = Nothing

toJsonText :: Aeson.ToJSON a => a -> Text
toJsonText = decodeUtf8 . BL.toStrict . Aeson.encode

{- | Open a CSV for row-at-a-time writing: write the header, hand the caller a
function that appends one row, and move the file into place at @path@ only
once the action finishes. Rows go to @path.partial@ until then, and if the
action throws or is cancelled the partial file is deleted, so @path@ never
holds a half-written run.
-}
withCsvOutput ::
    forall row a.
    (DefaultOrdered row, ToNamedRecord row) =>
    FilePath ->
    ((row -> IO ()) -> IO a) ->
    IO a
withCsvOutput path action = do
    let partial = path <> ".partial"
        writeAll = withBinaryFile partial WriteMode $ \h -> do
            -- cassava's headerOrder takes a value only for its type; both
            -- instances below ignore it, as cassava's own docs do.
            hPutBuilder h (encodeHeader (headerOrder (undefined :: row)))
            action (hPutBuilder h . encodeDefaultOrderedNamedRecord)
    result <- writeAll `onException` removePathForcibly partial
    renameFile partial path
    pure result

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
