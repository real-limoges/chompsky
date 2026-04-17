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
    , ProcessTrace (..)
    , FuzzySnapshot (..)
    , Span (..)
    , ScanHit (..)
    , TextEdit (..)
    , EditKind (..)
    , editKindTag
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

-- | A value extracted from natural language text by the scanner.
data ExtractedValue
    = -- | A simple categorical tag, e.g. @"Solar"@.
      TagValue Text
    | -- | A tag with an optional associated year, e.g. @"Roof" 2019@.
      TagWithYear Text (Maybe Int)
    | -- | A standalone 4-digit year.
      YearValue Int
    | -- | A dollar amount parsed from the text.
      AmountValue Double
    | -- | Free-form text captured after a trigger phrase.
      CapturedText Text
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- | Controls how multiple extracted values for the same category are combined.
data MergeStrategy
    = -- | Keep only unique values (set semantics).
      CollectUnique
    | -- | Keep the first value encountered; discard later ones.
      FirstWins
    | -- | Concatenate all values in encounter order.
      Concatenate
    deriving (Show, Eq)

-- | Parser confidence tier, derived from fuzzy inference over feature count and ambiguity.
data Confidence
    = -- | Score >= 0.65.
      High
    | -- | Score >= 0.35 and < 0.65.
      Medium
    | -- | Score < 0.35.
      Low
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)
    deriving anyclass (NFData)

{- | A quality flag raised by the triage stage. Each constructor identifies a
specific reason the extraction may need LLM review.
-}
data TriageDetail
    = -- | Long input with no extracted features.
      LongNoSignal {tdWordCount :: Int, tdFeatureCount :: Int}
    | -- | Input matches known ambiguous phrases for a category.
      AmbiguousPhrase {tdCategory :: Text, tdMatchedPhrases :: [Text]}
    | -- | High domain-vocabulary density but no extractions produced.
      HighVocabNoExtraction {tdCategory :: Text, tdWordCount :: Int, tdWords :: [Text]}
    | -- | Both positive and negative signals detected for the same category.
      ConflictingTags {tdCategory :: Text, tdPositive :: [Text], tdNegative :: [Text]}
    | -- | Text has a high ratio of non-ASCII characters.
      UnrecognizableText {tdNonAsciiRatio :: Double}
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Machine-readable tag for a 'TriageDetail' (e.g. @"long_remarks_no_signal"@).
triageDetailTag :: TriageDetail -> Text
triageDetailTag = \case
    LongNoSignal {} -> "long_remarks_no_signal"
    AmbiguousPhrase {} -> "ambiguous_condition"
    HighVocabNoExtraction {} -> "complex_renovation_narrative"
    ConflictingTags {} -> "conflicting_signals"
    UnrecognizableText {} -> "unrecognizable_text"

-- | Complete output of the extraction pipeline for a single input text.
data Extraction = Extraction
    { categories :: Map Text [ExtractedValue]
    -- ^ Extracted values keyed by category name.
    , needsLlm :: Bool
    -- ^ Whether triage flagged this extraction for LLM review.
    , llmReason :: Maybe Text
    -- ^ Human-readable reason for the LLM flag, if any.
    , triageDetails :: [TriageDetail]
    -- ^ All triage flags that fired.
    , parserConfidence :: Confidence
    -- ^ Fuzzy-inferred confidence tier.
    , confidenceScore :: Double
    -- ^ Raw numeric confidence score in @[0, 1]@.
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Newtype wrapper for text that has passed through the clean stage.
newtype CleanedText = CleanedText {unCleanedText :: Text}
    deriving (Show, Eq, Ord)
    deriving newtype (NFData, ToJSON)

-- | Per-stage snapshot of the pipeline for trace consumers (fugue, garcon).
data ProcessTrace = ProcessTrace
    { ptInput :: Text
    , ptNormalized :: Text
    , ptNormalizeEdits :: [TextEdit]
    , ptCleaned :: CleanedText
    , ptCleanEdits :: [TextEdit]
    , ptScanned :: Extraction
    , ptScanHits :: [ScanHit]
    , ptTriaged :: Extraction
    , ptFuzzy :: FuzzySnapshot
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Half-open character range within a specific text. @spEnd > spStart@.
data Span = Span
    { spStart :: Int
    , spEnd :: Int
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | A single scanner match — one firing of one parser entry.
data ScanHit = ScanHit
    { shEntryId :: Text
    {- ^ Stable identifier for the parser entry that produced this hit,
    of the form @"<category>/<tag>"@ with a @#N@ suffix on duplicates.
    -}
    , shCategory :: Text
    , shValue :: ExtractedValue
    , shSpan :: Span
    -- ^ Span in the *cleaned* text (input to the scanner).
    , shIsNegated :: Bool
    {- ^ True if the match was preceded by a negation word; such hits are
    dropped before merging into the final 'Extraction' but retained here
    so the viz can render them struck-through.
    -}
    , shNegationWord :: Maybe Text
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | A single edit produced by the Normalize or Clean stage.
data TextEdit = TextEdit
    { teKind :: EditKind
    , teSpan :: Span
    -- ^ Span in the pre-edit text (the input to the substep that emitted this edit).
    , teBefore :: Text
    , teAfter :: Text
    , teDetail :: Maybe Text
    -- ^ Optional extra context — e.g. the abbreviation key or boilerplate phrase.
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Kind of transformation applied by a 'TextEdit'.
data EditKind
    = AbbrevWord
    | AbbrevPhrase
    | PhoneStripped
    | EmailMasked
    | PunctCollapsed
    | WhitespaceCollapsed
    | BoilerplateStripped
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Machine-readable tag for an 'EditKind'.
editKindTag :: EditKind -> Text
editKindTag = \case
    AbbrevWord -> "abbrev_word"
    AbbrevPhrase -> "abbrev_phrase"
    PhoneStripped -> "phone_stripped"
    EmailMasked -> "email_masked"
    PunctCollapsed -> "punct_collapsed"
    WhitespaceCollapsed -> "whitespace_collapsed"
    BoilerplateStripped -> "boilerplate_stripped"

-- | Numeric score and tier produced by the fuzzy stage.
data FuzzySnapshot = FuzzySnapshot
    { fsScore :: Double
    , fsConfidence :: Confidence
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | A single row from the input CSV (id + raw remark text).
data InputRow = InputRow
    { rowId :: Text
    -- ^ Unique row identifier from the source CSV.
    , rowText :: Text
    -- ^ Raw remark text to be processed.
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Top-level application error, caught at the CLI boundary.
newtype ChompskyError = ChompskyError String
    deriving (Show)

instance Exception ChompskyError

-- | Events emitted by the backfill worker thread, consumed by the TUI.
data BackfillEvent
    = -- | CSV input reading has begun.
      InputFetchStarted
    | -- | CSV input reading complete; carries the total row count.
      InputFetchDone Int
    | -- | One row processed; carries whether it was flagged and its confidence tier.
      ItemProcessed Bool Confidence
    | -- | Output CSV writing has begun.
      CsvWriteStarted
    | -- | Output CSV writing complete.
      CsvWriteDone
    | -- | All work finished.
      BackfillFinished
    deriving (Show, Eq)

-- | Triage-stage configuration loaded from @triage.lua@.
data TriageConfig = TriageConfig
    { tcAmbiguousPhrases :: [(Text, [Text])]
    -- ^ (category, phrases) pairs for ambiguous phrase detection.
    , tcVocabWatchLists :: [(Text, [Text])]
    -- ^ (category, words) pairs for high-vocab-no-extraction detection.
    , tcConflictingTagPairs :: [(Text, [Text], [Text])]
    -- ^ (category, positive tags, negative tags) for conflict detection.
    , tcLongNoSignalThreshold :: Int
    -- ^ Word count above which a featureless input triggers 'LongNoSignal'.
    , tcVocabThreshold :: Int
    -- ^ Minimum domain-word hits to trigger 'HighVocabNoExtraction'.
    , tcUnrecognizableRatioThreshold :: Double
    -- ^ Non-ASCII ratio above which 'UnrecognizableText' fires.
    }
    deriving (Show, Eq)

-- | An 'Extraction' with no categories, no flags, and 'Low' confidence.
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

-- | Render a 'Confidence' tier as lowercase text (@"high"@, @"medium"@, @"low"@).
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

instance ToJSON FuzzySnapshot where
    toJSON fs =
        object
            [ "score" .= fsScore fs
            , "confidence" .= fsConfidence fs
            ]

instance ToJSON ProcessTrace where
    toJSON pt =
        object
            [ "input" .= ptInput pt
            , "normalized" .= ptNormalized pt
            , "normalize_edits" .= ptNormalizeEdits pt
            , "cleaned" .= ptCleaned pt
            , "clean_edits" .= ptCleanEdits pt
            , "scanned" .= ptScanned pt
            , "scan_hits" .= ptScanHits pt
            , "triaged" .= ptTriaged pt
            , "fuzzy" .= ptFuzzy pt
            ]

instance ToJSON Span where
    toJSON s = object ["start" .= spStart s, "end" .= spEnd s]

instance ToJSON ScanHit where
    toJSON h =
        object
            [ "entry_id" .= shEntryId h
            , "category" .= shCategory h
            , "value" .= shValue h
            , "span" .= shSpan h
            , "is_negated" .= shIsNegated h
            , "negation_word" .= shNegationWord h
            ]

instance ToJSON TextEdit where
    toJSON e =
        object
            [ "kind" .= editKindTag (teKind e)
            , "span" .= teSpan e
            , "before" .= teBefore e
            , "after" .= teAfter e
            , "detail" .= teDetail e
            ]
