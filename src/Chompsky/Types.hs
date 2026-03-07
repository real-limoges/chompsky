{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Domain types for the extraction pipeline.
module Chompsky.Types
    ( ExtractedValue (..)
    , MergeStrategy (..)
    , Confidence (..)
    , TriageDetail (..)
    , triageDetailTag
    , Extraction (..)
    , InputRow (..)
    , CleanedText (..)
    , BackfillEvent (..)
    , ChompskyError (..)
    , TriageConfig (..)
    , emptyExtraction
    , confidenceToText
    ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , object
    , withObject
    , withText
    , (.:)
    , (.:?)
    , (.=)
    )
import Data.Aeson.Types (Parser)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

data ExtractedValue
    = TagValue Text
    | TagWithYear Text (Maybe Int)
    | YearValue Int
    | AmountValue Double
    | CapturedText Text
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

data MergeStrategy
    = CollectUnique
    | FirstWins
    | Concatenate
    deriving (Show, Eq)

data Confidence = High | Medium | Low
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)
    deriving anyclass (NFData)

data TriageDetail
    = LongNoSignal {tdWordCount :: Int, tdFeatureCount :: Int}
    | AmbiguousPhrase {tdCategory :: Text, tdMatchedPhrases :: [Text]}
    | HighVocabNoExtraction {tdCategory :: Text, tdWordCount :: Int, tdWords :: [Text]}
    | ConflictingTags {tdCategory :: Text, tdPositive :: [Text], tdNegative :: [Text]}
    | UnrecognizableText {tdNonAsciiRatio :: Double}
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

triageDetailTag :: TriageDetail -> Text
triageDetailTag = \case
    LongNoSignal {} -> "long_remarks_no_signal"
    AmbiguousPhrase {} -> "ambiguous_condition"
    HighVocabNoExtraction {} -> "complex_renovation_narrative"
    ConflictingTags {} -> "conflicting_signals"
    UnrecognizableText {} -> "unrecognizable_text"

data Extraction = Extraction
    { categories :: Map Text [ExtractedValue]
    , needsLlm :: Bool
    , llmReason :: Maybe Text
    , triageDetails :: [TriageDetail]
    , parserConfidence :: Confidence
    , confidenceScore :: Double
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

newtype CleanedText = CleanedText {unCleanedText :: Text}
    deriving (Show, Eq, Ord)
    deriving newtype (NFData)

data InputRow = InputRow
    { rowId :: Text
    , rowText :: Text
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

newtype ChompskyError = ChompskyError String
    deriving (Show)

instance Exception ChompskyError

data BackfillEvent
    = InputFetchStarted
    | InputFetchDone Int
    | ItemProcessed Bool Confidence
    | CsvWriteStarted
    | CsvWriteDone
    | BackfillFinished
    deriving (Show, Eq)

data TriageConfig = TriageConfig
    { tcAmbiguousPhrases :: [(Text, [Text])]
    -- ^ (category, phrases) pairs for ambiguous phrase detection
    , tcVocabWatchLists :: [(Text, [Text])]
    -- ^ (category, words) pairs for high-vocab-no-extraction detection
    , tcConflictingTagPairs :: [(Text, [Text], [Text])]
    -- ^ (category, positive tags, negative tags) for conflict detection
    , tcLongNoSignalThreshold :: Int
    , tcVocabThreshold :: Int
    , tcUnrecognizableRatioThreshold :: Double
    }
    deriving (Show, Eq)

emptyExtraction :: Extraction
emptyExtraction =
    Extraction
        { categories = Map.empty
        , needsLlm = False
        , llmReason = Nothing
        , triageDetails = []
        , parserConfidence = Low
        , confidenceScore = 0.0
        }

confidenceToText :: Confidence -> Text
confidenceToText = \case
    High -> "high"
    Medium -> "medium"
    Low -> "low"

confidenceFromText :: Text -> Maybe Confidence
confidenceFromText = \case
    "high" -> Just High
    "medium" -> Just Medium
    "low" -> Just Low
    _ -> Nothing

instance ToJSON ExtractedValue where
    toJSON = \case
        TagValue t -> object ["type" .= ("tag" :: Text), "value" .= t]
        TagWithYear t yr -> object ["type" .= ("tag_with_year" :: Text), "value" .= t, "year" .= yr]
        YearValue yr -> object ["type" .= ("year" :: Text), "value" .= yr]
        AmountValue amt -> object ["type" .= ("amount" :: Text), "value" .= amt]
        CapturedText t -> object ["type" .= ("captured_text" :: Text), "value" .= t]

instance FromJSON ExtractedValue where
    parseJSON = withObject "ExtractedValue" $ \o -> do
        typ <- o .: "type" :: Parser Text
        case typ of
            "tag" -> TagValue <$> o .: "value"
            "tag_with_year" -> TagWithYear <$> o .: "value" <*> o .:? "year"
            "year" -> YearValue <$> o .: "value"
            "amount" -> AmountValue <$> o .: "value"
            "captured_text" -> CapturedText <$> o .: "value"
            _ -> fail $ "Unknown ExtractedValue type: " <> T.unpack typ

instance ToJSON TriageDetail where
    toJSON td =
        object $ case td of
            LongNoSignal wc fc ->
                [ "rule" .= triageDetailTag td
                , "word_count" .= wc
                , "feature_count" .= fc
                ]
            AmbiguousPhrase cat phrases ->
                [ "rule" .= triageDetailTag td
                , "category" .= cat
                , "matched_phrases" .= phrases
                ]
            HighVocabNoExtraction cat wc ws ->
                [ "rule" .= triageDetailTag td
                , "category" .= cat
                , "word_count" .= wc
                , "words" .= ws
                ]
            ConflictingTags cat pos neg ->
                [ "rule" .= triageDetailTag td
                , "category" .= cat
                , "positive" .= pos
                , "negative" .= neg
                ]
            UnrecognizableText ratio ->
                [ "rule" .= triageDetailTag td
                , "non_ascii_ratio" .= ratio
                ]

instance FromJSON TriageDetail where
    parseJSON = withObject "TriageDetail" $ \o -> do
        rule <- o .: "rule" :: Parser Text
        case rule of
            "long_remarks_no_signal" ->
                LongNoSignal <$> o .: "word_count" <*> o .: "feature_count"
            "ambiguous_condition" ->
                AmbiguousPhrase <$> o .: "category" <*> o .: "matched_phrases"
            "complex_renovation_narrative" ->
                HighVocabNoExtraction <$> o .: "category" <*> o .: "word_count" <*> o .: "words"
            "conflicting_signals" ->
                ConflictingTags <$> o .: "category" <*> o .: "positive" <*> o .: "negative"
            "unrecognizable_text" ->
                UnrecognizableText <$> o .: "non_ascii_ratio"
            _ -> fail $ "Unknown triage rule: " <> T.unpack rule

instance ToJSON Confidence where
    toJSON = toJSON . confidenceToText

instance FromJSON Confidence where
    parseJSON = withText "Confidence" $ \t ->
        maybe (fail $ "Unknown Confidence: " <> T.unpack t) pure (confidenceFromText t)

instance ToJSON Extraction where
    toJSON e =
        object
            [ "categories" .= categories e
            , "needs_llm" .= needsLlm e
            , "llm_reason" .= llmReason e
            , "triage_details" .= triageDetails e
            , "parser_confidence" .= parserConfidence e
            , "confidence_score" .= confidenceScore e
            ]

instance FromJSON Extraction where
    parseJSON = withObject "Extraction" $ \o ->
        Extraction
            <$> o .: "categories"
            <*> o .: "needs_llm"
            <*> o .:? "llm_reason"
            <*> ((o .:? "triage_details") <&> fromMaybe [])
            <*> o .: "parser_confidence"
            <*> o .: "confidence_score"
