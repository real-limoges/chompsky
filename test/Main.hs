{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (throwIO, try)
import Data.Aeson (FromJSON, Object, ToJSON, Value (..), decode, encode, toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isSuffixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import HsLua.Core qualified as Lua
import System.Directory (copyFile, createDirectoryIfMissing, getCurrentDirectory, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Chompsky.CLI (BackfillOpts (..), backfillWorker, withErrorBoundary)
import Chompsky.Config (AppConfig (..), loadConfig, wordReplacements)
import Chompsky.Config.ParserSpec
    ( ParserSpec (..)
    , ParserStrategy (..)
    , SpecEntry (..)
    , validateSpec
    )
import Chompsky.IO.Csv (ExtractionRow (..), ReviewRow (..), toExtractionRow, toReviewRow, withCsvOutput)
import Chompsky.IO.CsvInput (readInputCsv, readInputCsvLimit)
import Chompsky.IO.Workers (processStreaming)
import Chompsky.Pipeline (ProcessResult (..), processRemarkPure, processRemarkTraced)
import Chompsky.Pipeline.Fuzzy (fuzzyConfidence)
import Chompsky.Pipeline.Parser (ScannerEntry (..), allEntries)
import Chompsky.Pipeline.Scanner (hitsToExtraction, scanAll, scanHits)
import Chompsky.Pipeline.Triage (triage)
import Chompsky.TUI.State
    ( Phase (..)
    , TUIState (..)
    , initialState
    , updateState
    )
import Chompsky.TUI.Widgets (formatElapsed, formatEta, formatPct, formatRate)
import Chompsky.Types

main :: IO ()
main = do
    cwd <- getCurrentDirectory
    let configDir = cwd </> "config"
    cfg <- loadConfig configDir
    defaultMain $
        testGroup
            "Chompsky"
            [ typesTests
            , configTests cfg
            , parserSpecTests
            , pipelineTests cfg
            , triageTests cfg
            , csvBuildTests cfg
            , csvInputTests
            , fuzzyTests
            , traceTests cfg
            , parserBuildTests
            , tuiStateTests
            , tuiWidgetsTests
            , workersTests cfg
            , cliTests
            , backfillTests cfg
            , configSandboxTests
            , scanRoundTripTests cfg
            , traceSchemaTests cfg
            ]

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

typesTests :: TestTree
typesTests =
    testGroup
        "Types"
        [ testCase "emptyExtraction has no categories" $
            Map.null (categories emptyExtraction) @?= True
        , testCase "emptyExtraction is not flagged" $
            needsLlm emptyExtraction @?= False
        , testCase "emptyExtraction has Low confidence" $
            parserConfidence emptyExtraction @?= Low
        , testCase "confidenceToText High" $
            confidenceToText High @?= "high"
        , testCase "confidenceToText Medium" $
            confidenceToText Medium @?= "medium"
        , testCase "confidenceToText Low" $
            confidenceToText Low @?= "low"
        , testCase "triageDetailTag LongNoSignal" $
            triageDetailTag (LongNoSignal 100 0) @?= "long_remarks_no_signal"
        , testCase "triageDetailTag AmbiguousPhrase" $
            triageDetailTag (AmbiguousPhrase "category" ["assorted"]) @?= "ambiguous_phrase"
        , testCase "triageDetailTag HighVocabNoExtraction" $
            triageDetailTag (HighVocabNoExtraction "category" 5 ["laptop"]) @?= "high_vocab_no_extraction"
        , testCase "pre-2026-11-06 triage tags still decode" $ do
            decode "{\"rule\":\"ambiguous_condition\",\"category\":\"c\",\"matched_phrases\":[\"x\"]}"
                @?= Just (AmbiguousPhrase "c" ["x"])
            decode "{\"rule\":\"complex_renovation_narrative\",\"category\":\"c\",\"word_count\":4,\"words\":[\"x\"]}"
                @?= Just (HighVocabNoExtraction "c" 4 ["x"])
        , testCase "triageDetailTag ConflictingTags" $
            triageDetailTag (ConflictingTags "condition" ["Good"] ["Poor"]) @?= "conflicting_signals"
        , testCase "triageDetailTag UnrecognizableText" $
            triageDetailTag (UnrecognizableText 0.5) @?= "unrecognizable_text"
        , jsonRoundtripTests
        ]

jsonRoundtripTests :: TestTree
jsonRoundtripTests =
    testGroup
        "JSON roundtrips"
        [ testCase "TagValue" $ jsonRoundtrip (TagValue "Solar")
        , testCase "TagWithYear" $ jsonRoundtrip (TagWithYear "Roof" (Just 2020))
        , testCase "TagWithYear Nothing" $ jsonRoundtrip (TagWithYear "Roof" Nothing)
        , testCase "YearValue" $ jsonRoundtrip (YearValue 2021)
        , testCase "AmountValue" $ jsonRoundtrip (AmountValue 1500.50)
        , testCase "CapturedText" $ jsonRoundtrip (CapturedText "keep frozen")
        , testCase "Confidence High" $ jsonRoundtrip High
        , testCase "Confidence Low" $ jsonRoundtrip Low
        , testCase "TriageDetail LongNoSignal" $ jsonRoundtrip (LongNoSignal 80 0)
        , testCase "TriageDetail AmbiguousPhrase" $ jsonRoundtrip (AmbiguousPhrase "cond" ["as-is"])
        , testCase "TriageDetail ConflictingTags" $ jsonRoundtrip (ConflictingTags "c" ["A"] ["B"])
        , testCase "TriageDetail UnrecognizableText" $ jsonRoundtrip (UnrecognizableText 0.4)
        , testCase "Extraction" $
            jsonRoundtrip
                Extraction
                    { categories = Map.singleton "category" [TagValue "Electronics"]
                    , needsLlm = True
                    , llmReason = Just "ambiguous_phrase"
                    , triageDetails = [AmbiguousPhrase "condition" ["as-is"]]
                    , parserConfidence = High
                    , confidenceScore = 0.8
                    }
        ]

jsonRoundtrip :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> IO ()
jsonRoundtrip val = do
    let decoded = decode (encode val)
    decoded @?= Just val

-- ---------------------------------------------------------------------------
-- Config loading
-- ---------------------------------------------------------------------------

configTests :: AppConfig -> TestTree
configTests cfg =
    testGroup
        "Config"
        [ testCase "loads abbreviations"
            $ assertBool "wordReplacements should be non-empty"
            $ not (null (wordReplacements (abbreviationConfig cfg)))
        , testCase "loads parser specs"
            $ assertBool "parserSpecs should be non-empty"
            $ not (null (parserSpecs cfg))
        , testCase "loads at least 5 parser specs"
            $ assertBool "expected at least 5 specs"
            $ length (parserSpecs cfg) >= 5
        , testCase "triage config has reasonable thresholds" $ do
            let tc = triageConfig cfg
            tcLongNoSignalThreshold tc @?= 40
            tcVocabThreshold tc @?= 3
        ]

-- ---------------------------------------------------------------------------
-- ParserSpec validation
-- ---------------------------------------------------------------------------

parserSpecTests :: TestTree
parserSpecTests =
    testGroup
        "ParserSpec validation"
        [ testCase "valid phrase spec passes" $
            validateSpec validPhraseSpec @?= Right ()
        , testCase "phrase spec with empty phrases fails" $
            assertBool "should fail" $
                case validateSpec emptyPhraseSpec of
                    Left _ -> True
                    Right _ -> False
        , testCase "valid verbal spec passes" $
            validateSpec validVerbalSpec @?= Right ()
        , testCase "verbal spec without adverbs fails" $
            assertBool "should fail" $
                case validateSpec (validVerbalSpec {psAdverbs = []}) of
                    Left _ -> True
                    Right _ -> False
        , testCase "verbal spec without action verbs fails" $
            assertBool "should fail" $
                case validateSpec (validVerbalSpec {psActionVerbs = []}) of
                    Left _ -> True
                    Right _ -> False
        , testCase "blank phrase fails"
            $ assertLeft
            $ validateSpec validPhraseSpec {psEntries = [defSpecEntry {seTag = "Tag", sePhrases = ["ok", ""]}]}
        , testCase "whitespace-only bridge fails"
            $ assertLeft
            $ validateSpec
                validPhraseSpec
                    { psStrategy = MonetaryStrategy
                    , psEntries = [defSpecEntry {seTag = "Fee", seLabels = ["fee"], seBridges = ["  "]}]
                    }
        , testCase "blank tag fails for a non-verbal strategy"
            $ assertLeft
            $ validateSpec validPhraseSpec {psEntries = [defSpecEntry {sePhrases = ["phrase one"]}]}
        ]

assertLeft :: Either String () -> Assertion
assertLeft = \case
    Left _ -> pure ()
    Right () -> assertFailure "expected validation to fail"

-- All-empty SpecEntry; tests set only the fields under test via record update.
defSpecEntry :: SpecEntry
defSpecEntry =
    SpecEntry
        { seTag = ""
        , sePhrases = []
        , seSynonyms = []
        , seSuffixExtra = []
        , seTriggers = []
        , seLabels = []
        , seBridges = []
        , seFrequency = []
        , seTerminators = ""
        , seDirectPhrases = []
        }

validPhraseSpec :: ParserSpec
validPhraseSpec =
    ParserSpec
        { psVersion = 1
        , psCategory = "test"
        , psStrategy = PhraseStrategy
        , psEntries = [defSpecEntry {seTag = "Tag", sePhrases = ["phrase one"]}]
        , psAdverbs = []
        , psActionVerbs = []
        , psSuffixVerbs = []
        }

emptyPhraseSpec :: ParserSpec
emptyPhraseSpec = validPhraseSpec {psEntries = [defSpecEntry {seTag = "Tag"}]}

validVerbalSpec :: ParserSpec
validVerbalSpec =
    ParserSpec
        { psVersion = 1
        , psCategory = "test"
        , psStrategy = VerbalStrategy
        , psEntries = [defSpecEntry {seTag = "Tag", seSynonyms = ["synonym"]}]
        , psAdverbs = ["recently"]
        , psActionVerbs = ["inspected"]
        , psSuffixVerbs = ["inspected"]
        }

-- ---------------------------------------------------------------------------
-- Pipeline integration tests
-- ---------------------------------------------------------------------------

pipelineTests :: AppConfig -> TestTree
pipelineTests cfg =
    testGroup
        "Pipeline"
        [ testGroup
            "Phrase strategy"
            [ testCase "matches literal phrase" $ do
                let result = runPipeline cfg "large screen television and electronics"
                assertHasCategory "category" result
                assertHasTagValue "category" "Electronics" result
            , testCase "matches furniture phrase" $ do
                let result = runPipeline cfg "wooden desk and office furniture"
                assertHasCategory "category" result
                assertHasTagValue "category" "Furniture" result
            , testCase "no match on unrelated text" $ do
                let result = runPipeline cfg "the weather is nice today"
                assertNoCategory "category" result
            ]
        , testGroup
            "Trigger strategy"
            [ testCase "extracts acquisition year" $ do
                let result = runPipeline cfg "acquired in 2020"
                assertHasCategory "date_acquired" result
                assertHasYearValue "date_acquired" 2020 result
            , testCase "extracts disposal year" $ do
                let result = runPipeline cfg "sold in 2019"
                assertHasCategory "date_acquired" result
                assertHasYearValue "date_acquired" 2019 result
            , testCase "no year from unrelated text" $ do
                let result = runPipeline cfg "just a random remark"
                assertNoCategory "date_acquired" result
            ]
        , testGroup
            "Monetary strategy"
            [ testCase "extracts maintenance cost" $ do
                let result = runPipeline cfg "maintenance fee $500"
                assertHasCategory "cost" result
                assertHasAmountValue "cost" 500.0 result
            , testCase "extracts shipping cost" $ do
                let result = runPipeline cfg "shipping cost $25.99"
                assertHasCategory "cost" result
                assertHasAmountValue "cost" 25.99 result
            , testCase "frequency suffix after the amount" $
                assertHasAmountValue "cost" 500.0 (runPipeline cfg "maintenance fee $500 per month")
            , testCase "text after the amount that is not a frequency" $
                assertHasAmountValue "cost" 500.0 (runPipeline cfg "maintenance fee $500 for the year")
            , testCase "bridge word between label and amount" $
                assertHasAmountValue "cost" 500.0 (runPipeline cfg "maintenance fee is $500")
            , testCase "sentence period after the amount" $
                assertHasAmountValue "cost" 25.0 (runPipeline cfg "shipping cost $25. thanks")
            ]
        , testGroup
            "Capture strategy"
            [ testCase "captures handling note" $ do
                let result = runPipeline cfg "special instructions keep frozen and upright."
                assertHasCategory "note" result
                let vals = extractedValues "note" result
                assertBool "should have captured text" $
                    any isCapturedText vals
            , testCase "captures storage note" $ do
                let result = runPipeline cfg "store in cool dry place."
                assertHasCategory "note" result
            , testCase "trigger does not fire inside a longer word" $ do
                -- "store in" must not match the start of "store information".
                let result = runPipeline cfg "store information sheet."
                assertNoCategory "note" result
            , testCase "capture stops at a line break" $
                extractedValues "note" (runPipeline cfg "special instructions keep frozen\nship on monday")
                    @?= [CapturedText "keep frozen"]
            , testCase "capture stops at a single exclamation mark" $
                extractedValues "note" (runPipeline cfg "fragile handle gently! ship on monday")
                    @?= [CapturedText "handle gently"]
            ]
        , testGroup
            "Plural forms"
            [ testCase "a plural matches its singular literal" $
                assertHasTagValue "category" "Electronics" (runPipeline cfg "two electronic devices")
            , testCase "a plural does not match inside a longer word" $
                assertNoCategory "category" (runPipeline cfg "electronic devicesss")
            ]
        , testGroup
            "Verbal strategy"
            [ testCase "matches prefix form" $ do
                let result = runPipeline cfg "inspected warehouse"
                assertHasCategory "activity" result
                assertHasTag "activity" "Warehouse" result
            , testCase "matches suffix form" $ do
                let result = runPipeline cfg "warehouse inspected"
                assertHasCategory "activity" result
                assertHasTag "activity" "Warehouse" result
            , testCase "matches with adverb" $ do
                let result = runPipeline cfg "recently inspected warehouse"
                assertHasCategory "activity" result
                assertHasTag "activity" "Warehouse" result
            , testCase "matches with year" $ do
                let result = runPipeline cfg "inspected warehouse in 2021"
                assertBool "expected Warehouse with year 2021" $
                    TagWithYear "Warehouse" (Just 2021) `elem` extractedValues "activity" result
            , testCase "serviced works as a suffix verb" $
                assertHasTag "activity" "Equipment" (runPipeline cfg "equipment serviced")
            ]
        , testGroup
            "Negation"
            [ testCase "negated phrase produces no extraction" $ do
                let result = runPipeline cfg "no electronics here"
                assertNoCategory "category" result
            , testCase "not negated phrase still matches" $ do
                let result = runPipeline cfg "has electronics"
                assertHasCategory "category" result
            , testCase "negation does not cross a sentence break" $
                assertHasCategory "category" (runPipeline cfg "no damage. electronics on site")
            , testCase "negation does not cross a line break" $
                assertHasCategory "category" (runPipeline cfg "no damage\nelectronics on site")
            , testCase "negation still reaches across a comma" $
                assertNoCategory "category" (runPipeline cfg "no tools, electronics")
            ]
        , testGroup
            "Normalization"
            [ testCase "abbreviations are expanded before matching" $ do
                -- "qty" expands to "quantity" per config/abbreviations.lua
                let result = runPipeline cfg "qty of electronics in stock"
                unCleanedText (prCleanedText (prResult result)) @?= "quantity of electronics in stock"
                assertHasCategory "category" result
            ]
        , testGroup
            "Cleaning"
            [ testCase "phone numbers are stripped" $ do
                let result = runPipeline cfg "call 555-123-4567 about electronics"
                assertHasCategory "category" result
                -- phone number should not interfere with extraction
                let cleaned = unCleanedText (prCleanedText (prResult result))
                assertBool "phone should be stripped" $
                    not (T.isInfixOf "555-123-4567" cleaned)
            , testCase "emails are deleted" $ do
                let result = runPipeline cfg "contact user@example.com about electronics"
                assertHasCategory "category" result
                let cleaned = unCleanedText (prCleanedText (prResult result))
                assertBool "email should be deleted" $
                    not (T.isInfixOf "user@example.com" cleaned)
            , testCase "phone number with no separators is stripped" $
                cleanedOf cfg "call 5551234567 today" @?= "call today"
            , testCase "phone number with space separators is stripped" $
                cleanedOf cfg "call 555 123 4567 today" @?= "call today"
            , testCase "ten digits inside a longer number are kept" $
                cleanedOf cfg "serial 155512345678 here" @?= "serial 155512345678 here"
            , testCase "phone number with a parenthesized area code is stripped" $
                cleanedOf cfg "call (555) 123-4567 today" @?= "call today"
            , testCase "parenthesized area code with no space is stripped" $
                cleanedOf cfg "call (555)123-4567 today" @?= "call today"
            , testCase "line breaks survive as one newline" $
                cleanedOf cfg "electronics \n\n  furniture" @?= "electronics\nfurniture"
            , testCase "a single exclamation mark is kept, a run collapses" $
                cleanedOf cfg "fragile! very fragile!!!" @?= "fragile! very fragile!"
            ]
        , testGroup
            "Multiple extractions"
            [ testCase "extracts from multiple categories" $ do
                let result = runPipeline cfg "electronics inspected warehouse"
                assertHasCategory "category" result
                assertHasCategory "activity" result
            , testCase "extracts trigger and monetary with phrase" $ do
                let result = runPipeline cfg "electronics acquired in 2020 maintenance fee $300"
                assertHasCategory "category" result
                assertHasCategory "date_acquired" result
                assertHasCategory "cost" result
            ]
        ]

-- ---------------------------------------------------------------------------
-- Triage tests
-- ---------------------------------------------------------------------------

triageTests :: AppConfig -> TestTree
triageTests cfg =
    testGroup
        "Triage"
        [ testCase "short clean text is not flagged" $ do
            let result = runPipeline cfg "electronics"
            needsLlm (extraction result) @?= False
        , testCase "long text with no signal is flagged" $ do
            let longText = T.unwords (replicate 50 "unremarkable")
            let result = runPipeline cfg longText
            needsLlm (extraction result) @?= True
            assertBool "should have LongNoSignal detail" $
                any isLongNoSignal (triageDetails (extraction result))
        , testCase "confidence tier is set" $ do
            let result = runPipeline cfg "electronics furniture clothing"
            let ext = extraction result
            assertBool "confidence score should be positive" $
                confidenceScore ext > 0
        , testCase "ambiguous phrase triggers flag" $ do
            -- "assorted" is in the triage config's ambiguous phrases for "category"
            let ext = extraction (runPipeline cfg "pallet of assorted goods")
            triageDetails ext @?= [AmbiguousPhrase "category" ["assorted"]]
            needsLlm ext @?= True
        , testCase "watch words flag a remark the parsers missed" $ do
            let ext = extraction (runPipeline cfg "laptop, tablet, monitor and printer on the dock")
            assertBool "expected a HighVocabNoExtraction detail for category" $
                HighVocabNoExtraction "category" 4 ["laptop", "tablet", "monitor", "printer"]
                    `elem` triageDetails ext
        , testCase "conflicting tags trigger flag" $ do
            let tcfg = emptyTriageConfig {tcConflictingTagPairs = [("condition", ["Good"], ["Poor"])]}
                ext = emptyExtraction {categories = Map.singleton "condition" [TagValue "Good", TagValue "Poor"]}
                details = triageDetails (triage tcfg (CleanedText "good and poor") ext)
            details @?= [ConflictingTags "condition" ["Good"] ["Poor"]]
        , testCase "watch words count through trailing punctuation" $ do
            let tcfg =
                    emptyTriageConfig
                        { tcVocabWatchLists = [("renovation", ["roof", "hvac", "kitchen", "bath"])]
                        , tcVocabThreshold = 3
                        }
                details = triageDetails (triage tcfg (CleanedText "roof, hvac, kitchen, bath") emptyExtraction)
            details @?= [HighVocabNoExtraction "renovation" 4 ["roof", "hvac", "kitchen", "bath"]]
        ]

-- | A triage config under which no rule can fire; tests switch one rule on.
emptyTriageConfig :: TriageConfig
emptyTriageConfig =
    TriageConfig
        { tcAmbiguousPhrases = []
        , tcVocabWatchLists = []
        , tcConflictingTagPairs = []
        , tcLongNoSignalThreshold = 1000
        , tcVocabThreshold = 1000
        , tcUnrecognizableRatioThreshold = 1.0
        }

-- ---------------------------------------------------------------------------
-- CSV building tests
-- ---------------------------------------------------------------------------

csvBuildTests :: AppConfig -> TestTree
csvBuildTests cfg =
    testGroup
        "IO.Csv rows"
        [ testCase "extraction row has correct id and timestamp" $ do
            let row = InputRow "test-42" "electronics"
                er = toExtractionRow "2024-01-01" row (processRemarkPure cfg (rowText row))
            erRowId er @?= "test-42"
            erParsedAt er @?= "2024-01-01"
        , testCase "flagged row produces review row with matching id" $ do
            let row = InputRow "rev-1" (T.unwords (replicate 50 "unremarkable"))
                result = processRemarkPure cfg (rowText row)
            fmap rrRowId (toReviewRow row result) @?= Just "rev-1"
        , testCase "unflagged row produces no review row" $ do
            let row = InputRow "1" "electronics"
            isJust (toReviewRow row (processRemarkPure cfg (rowText row))) @?= False
        , testCase "review row carries the plain cleaned text" $ do
            let row = InputRow "rev-2" (T.unwords (replicate 50 "unremarkable"))
                result = processRemarkPure cfg (rowText row)
            fmap rrRemarksClean (toReviewRow row result) @?= Just (unCleanedText (prCleanedText result))
        , testCase "withCsvOutput writes header and rows, leaving no partial file" $
            withSystemTempDirectory "chompsky-csv" $ \dir -> do
                let path = dir </> "out.csv"
                    row = InputRow "1" "electronics"
                withCsvOutput path $ \write ->
                    write (toExtractionRow "t" row (processRemarkPure cfg (rowText row)))
                written <- TIO.readFile path
                take 1 (T.lines written) @?= ["id,extraction_json,needs_llm,confidence,confidence_score,parsed_at\r"]
                length (T.lines written) @?= 2
                files <- listDirectory dir
                files @?= ["out.csv"]
        , testCase "withCsvOutput leaves the old file and removes the partial on failure" $
            withSystemTempDirectory "chompsky-csv" $ \dir -> do
                let path = dir </> "out.csv"
                    row = InputRow "1" "electronics"
                TIO.writeFile path "previous run\n"
                outcome <-
                    try $ withCsvOutput path $ \write -> do
                        write (toExtractionRow "t" row (processRemarkPure cfg (rowText row)))
                        throwIO (ChompskyError "mid-run failure")
                case outcome of
                    Left (ChompskyError _) -> pure ()
                    Right () -> assertFailure "expected the failure to propagate"
                kept <- TIO.readFile path
                kept @?= "previous run\n"
                files <- listDirectory dir
                files @?= ["out.csv"]
        ]

-- ---------------------------------------------------------------------------
-- CSV input tests
-- ---------------------------------------------------------------------------

csvInputTests :: TestTree
csvInputTests =
    testGroup
        "IO.CsvInput"
        [ testCase "reads valid CSV" $ withTestCsv $ \path -> do
            rows <- readInputCsv path
            length rows @?= 3
        , testCase "preserves row ids" $ withTestCsv $ \path -> do
            rows <- readInputCsv path
            map rowId rows @?= ["1", "2", "3"]
        , testCase "preserves row text" $ withTestCsv $ \path -> do
            rows <- readInputCsv path
            case rows of
                (r : _) -> rowText r @?= "first remark"
                [] -> assertFailure "expected at least one row"
        , testCase "readInputCsvLimit respects limit" $ withTestCsv $ \path -> do
            rows <- readInputCsvLimit path 2
            length rows @?= 2
        , testCase "readInputCsvLimit with limit larger than file" $ withTestCsv $ \path -> do
            rows <- readInputCsvLimit path 100
            length rows @?= 3
        , testCase "leading byte-order mark is ignored" $
            withSystemTempDirectory "chompsky-test" $ \dir -> do
                let path = dir </> "bom.csv"
                BL.writeFile path "\xEF\xBB\xBFid,text\n1,first remark\n"
                rows <- readInputCsv path
                map rowId rows @?= ["1"]
        ]

withTestCsv :: (FilePath -> IO ()) -> IO ()
withTestCsv action =
    withSystemTempDirectory "chompsky-test" $ \dir -> do
        let path = dir </> "test.csv"
        BL.writeFile path "id,text\n1,first remark\n2,second remark\n3,third remark\n"
        action path

-- ---------------------------------------------------------------------------
-- Fuzzy confidence (hazy integration)
-- ---------------------------------------------------------------------------

fuzzyTests :: TestTree
fuzzyTests =
    testGroup
        "Fuzzy confidence"
        [ testCase "many features, no ambiguity → High" $ do
            let (score, tier) = fuzzyConfidence 8 0
            tier @?= High
            assertBool "score should be >= 0.65" (score >= 0.65)
        , testCase "some features, no ambiguity → High" $
            snd (fuzzyConfidence 3 0) @?= High
        , testCase "features at the clamp ceiling stay High" $ do
            snd (fuzzyConfidence 10 0) @?= High
            snd (fuzzyConfidence 50 0) @?= High
        , testCase "nothing extracted, no ambiguity → Low" $
            snd (fuzzyConfidence 0 0) @?= Low
        , testCase "nothing extracted never outscores one feature"
            $ assertBool "0 features with flags should not beat 1 feature"
            $ fst (fuzzyConfidence 0 2) <= fst (fuzzyConfidence 1 1)
        , testCase "some features, mild ambiguity → Medium" $ do
            let (score, tier) = fuzzyConfidence 3 1
            tier @?= Medium
            assertBool "score should be >= 0.35" (score >= 0.35)
        , testCase "some features, severe ambiguity → Low" $ do
            let (score, tier) = fuzzyConfidence 3 3
            tier @?= Low
            assertBool "score should be < 0.35" (score < 0.35)
        , testCase "many features, severe ambiguity → Low" $ do
            let (score, tier) = fuzzyConfidence 8 4
            tier @?= Low
            assertBool "score should be < 0.35" (score < 0.35)
        , testCase "severe ambiguity drives score down" $ do
            let (highAmb, _) = fuzzyConfidence 3 3
            let (lowAmb, _) = fuzzyConfidence 3 0
            assertBool "more ambiguity should lower score" (highAmb < lowAmb)
        , testCase "score is clamped within [0,1]" $ do
            let (score, _tier) = fuzzyConfidence 100 100
            assertBool "score should be <= 1.0" (score <= 1.0)
            assertBool "score should be >= 0.0" (score >= 0.0)
        ]

-- ---------------------------------------------------------------------------
-- Trace (Tier 2) tests
-- ---------------------------------------------------------------------------

traceTests :: AppConfig -> TestTree
traceTests cfg =
    testGroup
        "Trace"
        [ testGroup
            "scan hits"
            [ testCase "produces a hit for a literal phrase match" $ do
                let pt = processRemarkTraced cfg "electronics"
                let hits = ptScanHits pt
                assertBool "expected at least one hit" (not (null hits))
                assertBool "expected a non-negated electronics hit" $
                    any (\h -> shCategory h == "category" && not (shIsNegated h)) hits
            , testCase "entry_id follows <category>/<tag> format" $ do
                let pt = processRemarkTraced cfg "electronics"
                case filter (\h -> shCategory h == "category") (ptScanHits pt) of
                    (h : _) -> shEntryId h @?= "category/Electronics"
                    [] -> assertFailure "expected an electronics hit"
            , testCase "span is within the cleaned text" $ do
                let pt = processRemarkTraced cfg "electronics"
                let cleanedLen = T.length (unCleanedText (ptCleaned pt))
                case ptScanHits pt of
                    (h : _) -> do
                        assertBool "start non-negative" (spStart (shSpan h) >= 0)
                        assertBool "end > start" (spEnd (shSpan h) > spStart (shSpan h))
                        assertBool "end within cleaned text" (spEnd (shSpan h) <= cleanedLen)
                    [] -> assertFailure "expected at least one hit"
            ]
        , testGroup
            "negation surfaced"
            [ testCase "negated match appears in scan_hits with flag set" $ do
                let pt = processRemarkTraced cfg "no electronics here"
                assertBool "expected a negated hit for electronics" $
                    any
                        ( \h ->
                            shCategory h == "category"
                                && shIsNegated h
                                && shNegationWord h == Just "no"
                        )
                        (ptScanHits pt)
            , testCase "curly-apostrophe contraction negates" $ do
                let pt = processRemarkTraced cfg "we don\x2019t have electronics"
                map shNegationWord (filter ((== "category") . shCategory) (ptScanHits pt))
                    @?= [Just "don\x2019t"]
            , testCase "negated match is filtered from the final Extraction" $ do
                let pt = processRemarkTraced cfg "no electronics here"
                Map.member "category" (categories (ptScanned pt)) @?= False
            ]
        , testGroup
            "normalize edits"
            [ testCase "word-level abbreviation produces an AbbrevWord edit" $ do
                -- "qty" expands to "quantity" per config/abbreviations.lua
                let pt = processRemarkTraced cfg "qty of electronics"
                assertBool "expected an AbbrevWord edit for qty" $
                    any
                        ( \e ->
                            teKind e == AbbrevWord
                                && teBefore e == "qty"
                        )
                        (ptNormalizeEdits pt)
            , testCase "no edits on plain input" $ do
                let pt = processRemarkTraced cfg "electronics"
                ptNormalizeEdits pt @?= []
            , testCase "phrase-level abbreviation produces an AbbrevPhrase edit" $ do
                let pt = processRemarkTraced cfg "temp controlled warehouse"
                assertBool "expected an AbbrevPhrase edit for 'temp controlled'" $
                    any
                        (\e -> teKind e == AbbrevPhrase && teBefore e == "temp controlled")
                        (ptNormalizeEdits pt)
            ]
        , testGroup
            "clean edits"
            [ testCase "phone number produces a PhoneStripped edit" $ do
                let pt = processRemarkTraced cfg "call 555-123-4567 about electronics"
                case filter (\e -> teKind e == PhoneStripped) (ptCleanEdits pt) of
                    (e : _) -> do
                        teBefore e @?= "555-123-4567"
                        teAfter e @?= " "
                    [] -> assertFailure "expected a PhoneStripped edit"
            , testCase "email produces an EmailMasked edit" $ do
                let pt = processRemarkTraced cfg "mail user@example.com about electronics"
                case filter (\e -> teKind e == EmailMasked) (ptCleanEdits pt) of
                    (e : _) -> teBefore e @?= "user@example.com"
                    [] -> assertFailure "expected an EmailMasked edit"
            , testCase "repeated punctuation produces a PunctCollapsed edit" $ do
                let pt = processRemarkTraced cfg "electronics!!! here"
                assertBool "expected PunctCollapsed edit" $
                    any (\e -> teKind e == PunctCollapsed) (ptCleanEdits pt)
            , testCase "multi-space runs produce a WhitespaceCollapsed edit" $ do
                let pt = processRemarkTraced cfg "hello    world electronics"
                assertBool "expected WhitespaceCollapsed edit" $
                    any (\e -> teKind e == WhitespaceCollapsed) (ptCleanEdits pt)
            , testCase "boilerplate past min_position produces a BoilerplateStripped edit" $ do
                -- min_position is 50; make sure the phrase sits past that offset
                let pt =
                        processRemarkTraced
                            cfg
                            "electronics installed last year here some padding, all data subject to verification"
                case filter (\e -> teKind e == BoilerplateStripped) (ptCleanEdits pt) of
                    (e : _) -> teBefore e @?= "all data subject to verification"
                    [] -> assertFailure "expected a BoilerplateStripped edit"
            ]
        , testGroup
            "edit-log replay"
            [ testCase (T.unpack input) $ do
                let pt = processRemarkTraced cfg input
                replayEdits (ptNormalizeEdits pt) (T.toLower input) @?= Right (ptNormalized pt)
                replayEdits (ptCleanEdits pt) (T.toLower (ptNormalized pt))
                    @?= Right (unCleanedText (ptCleaned pt))
            | input <-
                [ "qty pcs"
                , "  electronics  "
                , "temp controlled whse qty pcs"
                , "call 555-123-4567 or 555.987.6543 about electronics"
                , "call (555) 123-4567 about\n\n electronics"
                , "mail a@b.com and c@d.org about inv!!! and   maint ***"
                , "electronics installed last year here some padding, all data subject to verification"
                ]
            ]
        ]

{- | Apply edits in order. Each edit's span must locate its 'teBefore' in the
text as it stands at that point; a mismatch is reported rather than applied.
-}
replayEdits :: [TextEdit] -> Text -> Either String Text
replayEdits edits t0 = foldl step (Right t0) edits
    where
        step acc e = acc >>= apply e
        apply e t =
            let Span s end = teSpan e
                found = T.take (end - s) (T.drop s t)
             in if found == teBefore e
                    then Right (T.take s t <> teAfter e <> T.drop end t)
                    else Left (show (teKind e) <> " at " <> show (s, end) <> " found " <> show found)

-- ---------------------------------------------------------------------------
-- Parser.Build: stable entry IDs
-- ---------------------------------------------------------------------------

parserBuildTests :: TestTree
parserBuildTests =
    testGroup
        "Parser.Build entry IDs"
        [ testCase "single tag gets <category>/<tag> with no suffix" $ do
            let spec =
                    ParserSpec
                        { psVersion = 1
                        , psCategory = "fruit"
                        , psStrategy = PhraseStrategy
                        , psEntries = [defSpecEntry {seTag = "apple", sePhrases = ["apple"]}]
                        , psAdverbs = []
                        , psActionVerbs = []
                        , psSuffixVerbs = []
                        }
            map seId (allEntries [spec]) @?= ["fruit/apple"]
        , testCase "duplicate tags get #N suffix (2-based)" $ do
            let mkEntry t = defSpecEntry {seTag = t, sePhrases = [t]}
                spec =
                    ParserSpec
                        { psVersion = 1
                        , psCategory = "fruit"
                        , psStrategy = PhraseStrategy
                        , psEntries = [mkEntry "apple", mkEntry "apple", mkEntry "apple"]
                        , psAdverbs = []
                        , psActionVerbs = []
                        , psSuffixVerbs = []
                        }
            map seId (allEntries [spec]) @?= ["fruit/apple", "fruit/apple#2", "fruit/apple#3"]
        , testCase "distinct tags keep their base IDs" $ do
            let spec =
                    ParserSpec
                        { psVersion = 1
                        , psCategory = "fruit"
                        , psStrategy = PhraseStrategy
                        , psEntries =
                            [ defSpecEntry {seTag = "apple", sePhrases = ["apple"]}
                            , defSpecEntry {seTag = "pear", sePhrases = ["pear"]}
                            ]
                        , psAdverbs = []
                        , psActionVerbs = []
                        , psSuffixVerbs = []
                        }
            map seId (allEntries [spec]) @?= ["fruit/apple", "fruit/pear"]
        , testCase "two entries with the same tag give two hits with different IDs" $ do
            let spec =
                    fruitSpec
                        [ defSpecEntry {seTag = "apple", sePhrases = ["apple"]}
                        , defSpecEntry {seTag = "apple", sePhrases = ["apple pie"]}
                        ]
            map shEntryId (scanHits (allEntries [spec]) "apple pie")
                @?= ["fruit/apple", "fruit/apple#2"]
        , testCase "the longest literal wins whatever the config order" $ do
            let spec = fruitSpec [defSpecEntry {seTag = "pie", sePhrases = ["apple", "apple pie"]}]
            map shSpan (scanHits (allEntries [spec]) "apple pie") @?= [Span 0 9]
        ]
    where
        fruitSpec entries =
            ParserSpec
                { psVersion = 1
                , psCategory = "fruit"
                , psStrategy = PhraseStrategy
                , psEntries = entries
                , psAdverbs = []
                , psActionVerbs = []
                , psSuffixVerbs = []
                }

-- ---------------------------------------------------------------------------
-- TUI state transitions
-- ---------------------------------------------------------------------------

fixedTime :: UTCTime
fixedTime = read "2026-04-17 00:00:00 UTC"

tuiStateTests :: TestTree
tuiStateTests =
    testGroup
        "TUI state"
        [ testCase "initial state is Fetching with zeroed counters" $ do
            tsPhase initialState @?= Fetching
            tsTotal initialState @?= 0
            tsProcessed initialState @?= 0
            tsFinished initialState @?= False
        , testCase "InputFetchDone moves to Processing and records total" $ do
            let st = updateState fixedTime (InputFetchDone 7) initialState
            tsPhase st @?= Processing
            tsTotal st @?= 7
            tsStartTime st @?= Just fixedTime
        , testCase "ItemProcessed increments counters by confidence tier" $ do
            let st0 = updateState fixedTime (InputFetchDone 3) initialState
                st1 = updateState fixedTime (ItemProcessed True High) st0
                st2 = updateState fixedTime (ItemProcessed False Medium) st1
                st3 = updateState fixedTime (ItemProcessed True Low) st2
            tsProcessed st3 @?= 3
            tsFlagged st3 @?= 2
            tsHighConf st3 @?= 1
            tsMedConf st3 @?= 1
            tsLowConf st3 @?= 1
        , testCase "BackfillFinished marks Done and finished" $ do
            let st = updateState fixedTime BackfillFinished initialState
            tsPhase st @?= Done
            tsFinished st @?= True
        , testCase "BackfillFailed marks Failed and finished" $ do
            let st = updateState fixedTime (BackfillFailed "boom") initialState
            tsPhase st @?= Failed "boom"
            tsFinished st @?= True
        ]

-- ---------------------------------------------------------------------------
-- TUI widget formatters
-- ---------------------------------------------------------------------------

tuiWidgetsTests :: TestTree
tuiWidgetsTests =
    testGroup
        "TUI widgets"
        [ testCase "formatElapsed zero-pads minutes and seconds" $ do
            formatElapsed 0 @?= "00:00"
            formatElapsed 65 @?= "01:05"
            formatElapsed 3599 @?= "59:59"
        , testCase "formatRate returns a dash until elapsed > 0" $
            formatRate 100 0 @?= "-"
        , testCase "formatRate reports rows/sec with no decimals" $
            formatRate 100 10 @?= "10 rows/sec"
        , testCase "formatEta returns --- until 5 items processed" $ do
            formatEta 100 0 10 @?= "---"
            formatEta 100 4 10 @?= "---"
        , testCase "formatEta returns --- when elapsed is zero" $
            formatEta 100 10 0 @?= "---"
        , testCase "formatEta projects mm:ss from current rate" $
            -- 10 items in 10s -> 1 row/s; 90 remaining -> 90s -> 01:30
            formatEta 100 10 10 @?= "01:30"
        , testCase "formatPct handles empty denominator" $
            formatPct 5 0 @?= "0.0%"
        , testCase "formatPct reports one decimal" $
            formatPct 1 4 @?= "25.0%"
        ]

-- ---------------------------------------------------------------------------
-- IO.Workers: processStreaming
-- ---------------------------------------------------------------------------

workersTests :: AppConfig -> TestTree
workersTests cfg =
    testGroup
        "IO.Workers.processStreaming"
        [ testCase "calls back in input row order" $ do
            let rows =
                    [ InputRow "a" "electronics"
                    , InputRow "b" "qty of pcs"
                    , InputRow "c" "no electronics"
                    ]
            seenRef <- newIORef []
            processStreaming cfg (`mapM_` rows) (\row _ -> modifyIORef' seenRef (rowId row :))
            seen <- readIORef seenRef
            reverse seen @?= ["a", "b", "c"]
        , testCase "invokes callback once per row" $ do
            let rows = [InputRow (T.pack (show i)) "electronics" | i <- [1 .. 5 :: Int]]
            counter <- newIORef (0 :: Int)
            processStreaming cfg (`mapM_` rows) (\_ _ -> modifyIORef' counter (+ 1))
            n <- readIORef counter
            n @?= 5
        , testCase "empty input never calls back" $ do
            counter <- newIORef (0 :: Int)
            processStreaming cfg (\_ -> pure ()) (\_ _ -> modifyIORef' counter (+ 1))
            n <- readIORef counter
            n @?= 0
        , testCase "callback receives matching row and extraction" $ do
            let rows = [InputRow "only" "electronics"]
            seenRef <- newIORef []
            processStreaming cfg (`mapM_` rows) $ \row result ->
                modifyIORef' seenRef ((rowId row, categories (prExtraction result)) :)
            seen <- readIORef seenRef
            case seen of
                [(rid, cats)] -> do
                    rid @?= "only"
                    assertBool "expected category map populated" (Map.member "category" cats)
                _ -> assertFailure "expected exactly one callback invocation"
        ]

-- ---------------------------------------------------------------------------
-- CLI error boundary
-- ---------------------------------------------------------------------------

cliTests :: TestTree
cliTests =
    testGroup
        "CLI"
        [ testCase "withErrorBoundary passes value through on success" $ do
            x <- withErrorBoundary "test" (pure (42 :: Int))
            x @?= 42
        , testCase "withErrorBoundary is transparent for IO effects" $ do
            ref <- newIORef (0 :: Int)
            withErrorBoundary "test" (modifyIORef' ref (+ 1))
            n <- readIORef ref
            n @?= 1
        ]

backfillTests :: AppConfig -> TestTree
backfillTests cfg =
    testGroup
        "CLI backfill worker"
        [ testCase "input without a text column throws and never finishes" $
            withSystemTempDirectory "chompsky-test" $ \dir -> do
                let input = dir </> "in.csv"
                BL.writeFile input "id,remark\n1,electronics\n"
                (events, outcome) <- runWorker (backfillOptsIn dir input)
                case outcome of
                    Left (ChompskyError _) -> pure ()
                    Right () -> assertFailure "expected the worker to throw"
                assertBool "worker must not report BackfillFinished" (BackfillFinished `notElem` events)
        , testCase "writes both files and finishes" $
            withSystemTempDirectory "chompsky-test" $ \dir -> do
                let input = dir </> "in.csv"
                    opts = backfillOptsIn dir input
                BL.writeFile input "id,text\n1,electronics\n"
                (events, outcome) <- runWorker opts
                either (\(ChompskyError msg) -> assertFailure msg) pure outcome
                lastMaybe events @?= Just BackfillFinished
                extractions <- TIO.readFile (bfOutput opts)
                -- parsed_at is the last column, and an ISO-8601 time has no comma.
                case T.lines extractions of
                    [_header, dataLine] -> do
                        let parsedAt = T.strip (last (T.splitOn "," dataLine))
                        assertBool ("parsed_at should be ISO-8601: " <> T.unpack parsedAt) $
                            isJust (iso8601ParseM (T.unpack parsedAt) :: Maybe UTCTime)
                    other -> assertFailure ("expected a header and one row, got " <> show other)
                reviews <- BL.readFile (bfReviews opts)
                reviews @?= "id,remarks_clean,triage_reason,triage_details,partial_extraction\r\n"
        ]
    where
        backfillOptsIn dir input =
            BackfillOpts {bfInput = input, bfOutput = dir </> "out.csv", bfReviews = dir </> "reviews.csv"}
        runWorker opts = do
            ref <- newIORef []
            outcome <- try (backfillWorker cfg opts (\ev -> modifyIORef' ref (ev :)))
            events <- reverse <$> readIORef ref
            pure (events, outcome)
        lastMaybe xs = if null xs then Nothing else Just (last xs)

-- ---------------------------------------------------------------------------
-- Config sandbox and validation
-- ---------------------------------------------------------------------------

configSandboxTests :: TestTree
configSandboxTests =
    testGroup
        "Config sandbox"
        [ testCase "the shipped config loads" $ do
            dir <- (</> "config") <$> getCurrentDirectory
            _ <- loadConfig dir
            pure ()
        , testCase "abbreviations.lua cannot reach the standard library" $
            assertConfigRejected "abbreviations.lua" "os.exit(1)\nreturn { words = {}, phrases = {} }"
        , testCase "triage.lua cannot call print" $
            assertConfigRejected "triage.lua" "print(\"hi\")\nreturn {}"
        , testCase "blank boilerplate phrase is rejected" $
            assertConfigRejected "boilerplate.lua" "return { min_position = 0, phrases = { \"\" } }"
        , testCase "blank abbreviation key is rejected" $
            assertConfigRejected
                "abbreviations.lua"
                "return { words = {}, phrases = { { match = \"\", replace = \"x\" } } }"
        , testCase "a config that loops is stopped" $
            assertConfigRejected "boilerplate.lua" "while true do end\nreturn { min_position = 0, phrases = {} }"
        , testCase "a misspelled top-level key is rejected" $
            assertConfigRejected "boilerplate.lua" "return { min_postion = 0, phrases = {} }"
        , testCase "a misspelled triage entry key is rejected" $
            assertConfigRejected
                "triage.lua"
                "return { ambiguous_phrases = { { category = \"c\", phrase = { \"x\" } } },\
                \ vocab_watch_lists = {}, conflicting_tag_pairs = {}, long_no_signal_threshold = 40,\
                \ vocab_threshold = 3, unrecognizable_ratio_threshold = 0.3 }"
        , testCase "a misspelled parser spec entry key is rejected" $
            assertConfigRejected
                "parsers/cost.lua"
                "return { version = 1, category = \"cost\", strategy = \"monetary\",\
                \ entries = { { tag = \"Fee\", labels = { \"fee\" }, bridge = { \"is\" } } } }"
        , testCase "an unsupported parser spec version is rejected" $
            assertConfigRejected
                "parsers/cost.lua"
                "return { version = 2, category = \"cost\", strategy = \"monetary\",\
                \ entries = { { tag = \"Fee\", labels = { \"fee\" } } } }"
        , testCase "uppercase abbreviation keys still expand" $
            withConfigOverride "abbreviations.lua" "return { words = { QTY = \"quantity\" }, phrases = {} }" $ \dir -> do
                cfg <- loadConfig dir
                unCleanedText (prCleanedText (processRemarkPure cfg "qty")) @?= "quantity"
        ]

-- | Copy the shipped config into a temp dir, replace one file, and run the action on it.
withConfigOverride :: FilePath -> BL.ByteString -> (FilePath -> IO ()) -> IO ()
withConfigOverride name contents action =
    withSystemTempDirectory "chompsky-config" $ \tmp -> do
        src <- (</> "config") <$> getCurrentDirectory
        let dst = tmp </> "config"
        createDirectoryIfMissing True (dst </> "parsers")
        topFiles <- filter (".lua" `isSuffixOf`) <$> listDirectory src
        mapM_ (\f -> copyFile (src </> f) (dst </> f)) topFiles
        parserFiles <- listDirectory (src </> "parsers")
        mapM_ (\f -> copyFile (src </> "parsers" </> f) (dst </> "parsers" </> f)) parserFiles
        BL.writeFile (dst </> name) contents
        action dst

assertConfigRejected :: FilePath -> BL.ByteString -> Assertion
assertConfigRejected name contents =
    withConfigOverride name contents $ \dir -> do
        outcome <- try (loadConfig dir)
        case outcome of
            Left (_ :: Lua.Exception) -> pure ()
            Right _ -> assertFailure ("expected " <> name <> " to be rejected")

-- ---------------------------------------------------------------------------
-- Scanner round-trip: hitsToExtraction . scanHits == fst . scanAll
-- ---------------------------------------------------------------------------

scanRoundTripTests :: AppConfig -> TestTree
scanRoundTripTests cfg =
    testGroup
        "Scanner round-trip"
        [ testCase (T.unpack label) $ do
            let entries = allEntries (parserSpecs cfg)
                hits = scanHits entries input
                fromHits = hitsToExtraction hits
                fromAll = fst (scanAll entries input)
            categories fromHits @?= categories fromAll
        | (label, input) <-
            [ ("electronics only", "electronics")
            , ("negated", "no electronics here")
            , ("multi-category", "electronics acquired in 2020 maintenance fee $500 per month")
            , ("empty", "")
            , ("no match", "nothing parsable here")
            ]
        ]

-- ---------------------------------------------------------------------------
-- ProcessTrace JSON schema: pin the top-level key set fugue depends on.
-- ---------------------------------------------------------------------------

traceSchemaKeys :: [Text]
traceSchemaKeys =
    [ "input"
    , "normalized"
    , "normalize_edits"
    , "cleaned"
    , "clean_edits"
    , "scanned"
    , "scan_hits"
    , "triaged"
    , "fuzzy"
    ]

traceSchemaTests :: AppConfig -> TestTree
traceSchemaTests cfg =
    testGroup
        "ProcessTrace JSON schema"
        [ testCase "top-level keys match the pinned schema" $ do
            let pt = processRemarkTraced cfg "electronics installed in 2020"
            assertObjectKeys "ProcessTrace" (toJSON pt) traceSchemaKeys
        , testCase "fuzzy object carries score and confidence" $ do
            let pt = processRemarkTraced cfg "electronics"
            assertObjectKeys "fuzzy" (toJSON (ptFuzzy pt)) ["confidence", "score"]
        , testCase "scan_hits entry carries the fugue-facing keys" $ do
            let pt = processRemarkTraced cfg "electronics"
            case ptScanHits pt of
                [] -> assertFailure "expected at least one hit"
                (h : _) ->
                    assertObjectKeys
                        "scan hit"
                        (toJSON h)
                        ["category", "entry_id", "is_negated", "negation_word", "span", "value"]
        , testCase "normalize_edits entry carries the fugue-facing keys" $ do
            let pt = processRemarkTraced cfg "qty of electronics"
            case ptNormalizeEdits pt of
                [] -> assertFailure "expected at least one normalize edit"
                (e : _) ->
                    assertObjectKeys
                        "edit"
                        (toJSON e)
                        ["after", "before", "detail", "kind", "span"]
        ]

objectKeys :: Object -> [Text]
objectKeys = sort . map Key.toText . KeyMap.keys

assertObjectKeys :: String -> Value -> [Text] -> Assertion
assertObjectKeys label val expected = case val of
    Object o -> objectKeys o @?= sort expected
    _ -> assertFailure (label <> ": expected a JSON object")

-- ---------------------------------------------------------------------------
-- Pipeline test helpers
-- ---------------------------------------------------------------------------

data PipelineResult = PipelineResult
    { extraction :: Extraction
    , prResult :: ProcessResult
    }

runPipeline :: AppConfig -> Text -> PipelineResult
runPipeline cfg input =
    let pr = processRemarkPure cfg input
     in PipelineResult (prExtraction pr) pr

extractedValues :: Text -> PipelineResult -> [ExtractedValue]
extractedValues cat pr =
    fromMaybe [] (Map.lookup cat (categories (extraction pr)))

assertHasCategory :: Text -> PipelineResult -> IO ()
assertHasCategory cat pr =
    assertBool ("expected category: " <> T.unpack cat) $
        Map.member cat (categories (extraction pr))

assertNoCategory :: Text -> PipelineResult -> IO ()
assertNoCategory cat pr =
    assertBool ("unexpected category: " <> T.unpack cat) $
        not (Map.member cat (categories (extraction pr)))

assertHasTagValue :: Text -> Text -> PipelineResult -> IO ()
assertHasTagValue cat tag pr =
    assertBool ("expected TagValue " <> T.unpack tag <> " in " <> T.unpack cat) $
        TagValue tag `elem` extractedValues cat pr

assertHasTag :: Text -> Text -> PipelineResult -> IO ()
assertHasTag cat tag pr =
    assertBool ("expected tag " <> T.unpack tag <> " in " <> T.unpack cat) $
        any (hasTag tag) (extractedValues cat pr)

assertHasYearValue :: Text -> Int -> PipelineResult -> IO ()
assertHasYearValue cat yr pr =
    assertBool ("expected YearValue " <> show yr <> " in " <> T.unpack cat) $
        YearValue yr `elem` extractedValues cat pr

assertHasAmountValue :: Text -> Double -> PipelineResult -> IO ()
assertHasAmountValue cat amt pr =
    assertBool ("expected AmountValue " <> show amt <> " in " <> T.unpack cat) $
        any (isAmountClose amt) (extractedValues cat pr)

cleanedOf :: AppConfig -> Text -> Text
cleanedOf cfg = unCleanedText . prCleanedText . processRemarkPure cfg

hasTag :: Text -> ExtractedValue -> Bool
hasTag t (TagValue v) = v == t
hasTag t (TagWithYear v _) = v == t
hasTag _ _ = False

isCapturedText :: ExtractedValue -> Bool
isCapturedText (CapturedText _) = True
isCapturedText _ = False

isAmountClose :: Double -> ExtractedValue -> Bool
isAmountClose expected (AmountValue actual) = abs (expected - actual) < 0.01
isAmountClose _ _ = False

isLongNoSignal :: TriageDetail -> Bool
isLongNoSignal (LongNoSignal {}) = True
isLongNoSignal _ = False
