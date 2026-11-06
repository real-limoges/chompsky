{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Post-extraction triage: flag items for LLM review and score confidence.
module Chompsky.Pipeline.Triage
    ( triage
    ) where

import Data.Char (isAlphaNum, isAscii)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Chompsky.Pipeline.Fuzzy (fuzzyConfidence)
import Chompsky.Types

triage :: TriageConfig -> CleanedText -> Extraction -> Extraction
triage cfg (CleanedText cleanedText) ext =
    ext
        { needsLlm = not (null triggered)
        , llmReason = reason
        , triageDetails = triggered
        , parserConfidence = tier
        , confidenceScore = score
        }
    where
        wordList = T.words cleanedText
        wordCount = length wordList
        (!textLen, !nonAsciiCount) =
            T.foldl' (\(!l, !n) c -> (l + 1, if isAscii c then n else n + 1)) (0, 0) cleanedText
        triggered =
            concat
                [ checkLongNoSignal (tcLongNoSignalThreshold cfg) wordCount ext
                , checkAmbiguousPhrases (tcAmbiguousPhrases cfg) cleanedText
                , checkHighVocabNoExtraction (tcVocabWatchLists cfg) (tcVocabThreshold cfg) wordList ext
                , checkConflictingTags (tcConflictingTagPairs cfg) ext
                , checkUnrecognizable (tcUnrecognizableRatioThreshold cfg) textLen nonAsciiCount
                ]
        (score, tier) = fuzzyConfidence (countFeatures ext) (length triggered)
        reason
            | null triggered = Nothing
            | otherwise = Just (T.intercalate "," (map triageDetailTag triggered))

countFeatures :: Extraction -> Int
countFeatures = sum . map length . Map.elems . categories

-- | Fire when the text is long (word count over threshold) yet nothing was extracted.
checkLongNoSignal :: Int -> Int -> Extraction -> [TriageDetail]
checkLongNoSignal threshold wordCount ext
    | wordCount > threshold && fc == 0 = [LongNoSignal wordCount fc]
    | otherwise = []
    where
        fc = countFeatures ext

-- | Fire when a known ambiguous phrase for a category appears in the text.
checkAmbiguousPhrases :: [(Text, [Text])] -> Text -> [TriageDetail]
checkAmbiguousPhrases pairs text =
    mapMaybe toDetail pairs
    where
        toDetail (cat, phrases) = case filter (`containsPhrase` text) phrases of
            [] -> Nothing
            matched -> Just (AmbiguousPhrase cat matched)

{- | Fire when a category's watch words appear often (over threshold) but it
extracted nothing. Punctuation is trimmed from each word before the lookup,
so @"roof,"@ counts as @"roof"@.
-}
checkHighVocabNoExtraction :: [(Text, [Text])] -> Int -> [Text] -> Extraction -> [TriageDetail]
checkHighVocabNoExtraction watchLists threshold wordList ext =
    mapMaybe toDetail watchLists
    where
        normalizedWords = map (T.toLower . T.dropAround (not . isAlphaNum)) wordList
        toDetail (cat, ws)
            | matchCount > threshold && null (Map.findWithDefault [] cat (categories ext)) =
                Just (HighVocabNoExtraction cat matchCount matched)
            | otherwise = Nothing
            where
                wordSet = Set.fromList ws
                matched = filter (`Set.member` wordSet) normalizedWords
                matchCount = length matched

-- | Fire when a category holds both a positive and a negative tag at once.
checkConflictingTags :: [(Text, [Text], [Text])] -> Extraction -> [TriageDetail]
checkConflictingTags pairs ext =
    mapMaybe toDetail pairs
    where
        toDetail (cat, posTags, negTags)
            | not (null pos) && not (null neg) = Just (ConflictingTags cat pos neg)
            | otherwise = Nothing
            where
                tagTexts = [t | TagValue t <- Map.findWithDefault [] cat (categories ext)]
                pos = filter (`elem` posTags) tagTexts
                neg = filter (`elem` negTags) tagTexts

-- | Fire when the non-ASCII character ratio exceeds threshold (likely garbled text).
checkUnrecognizable :: Double -> Int -> Int -> [TriageDetail]
checkUnrecognizable threshold textLen nonAsciiCount
    | textLen == 0 = []
    | ratio > threshold = [UnrecognizableText ratio]
    | otherwise = []
    where
        ratio = fromIntegral nonAsciiCount / fromIntegral textLen

{- | Case-insensitive substring search that matches only on word boundaries, so
@"air"@ does not match inside @"repair"@. One of several sites with this policy;
see docs/reference/internal/design-decisions.md ("Word-boundary matching is
duplicated on purpose").
-}
containsPhrase :: Text -> Text -> Bool
containsPhrase phrase text = go (T.toLower text)
    where
        go t =
            let (before, rest) = T.breakOn phrase t
                after = T.drop (T.length phrase) rest
                prevOk = T.null before || not (isAlphaNum (T.last before))
                nextOk = T.null after || not (isAlphaNum (T.head after))
             in not (T.null rest) && ((prevOk && nextOk) || go (T.drop 1 rest))
