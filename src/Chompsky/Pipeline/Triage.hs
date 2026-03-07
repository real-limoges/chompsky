{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Post-extraction triage: flag items for LLM review and score confidence.
module Chompsky.Pipeline.Triage
    ( triage
    ) where

import Data.Char (isAlphaNum, isAscii)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Chompsky.Pipeline.Fuzzy (fuzzyConfidence)
import Chompsky.Types

triage :: TriageConfig -> CleanedText -> Extraction -> Extraction
triage cfg (CleanedText cleanedText) ext =
    let wordList = T.words cleanedText
        wordCount = length wordList
        (!textLen, !nonAsciiCount) =
            T.foldl' (\(!l, !n) c -> (l + 1, if isAscii c then n else n + 1)) (0, 0) cleanedText
        rules =
            [ checkLongNoSignal (tcLongNoSignalThreshold cfg) wordCount ext
            , checkAmbiguousPhrases (tcAmbiguousPhrases cfg) cleanedText
            , checkHighVocabNoExtraction (tcVocabWatchLists cfg) (tcVocabThreshold cfg) wordList ext
            , checkConflictingTags (tcConflictingTagPairs cfg) ext
            , checkUnrecognizable (tcUnrecognizableRatioThreshold cfg) textLen nonAsciiCount
            ]
        triggered = concatMap catMaybes rules
        featureCount = countFeatures ext
        (score, tier) = fuzzyConfidence featureCount (length triggered)
     in case triggered of
            [] -> ext {triageDetails = [], parserConfidence = tier, confidenceScore = score}
            details ->
                ext
                    { needsLlm = True
                    , llmReason = Just (T.intercalate "," (map triageDetailTag details))
                    , triageDetails = details
                    , parserConfidence = tier
                    , confidenceScore = score
                    }

countFeatures :: Extraction -> Int
countFeatures = sum . map length . Map.elems . categories

checkLongNoSignal :: Int -> Int -> Extraction -> [Maybe TriageDetail]
checkLongNoSignal threshold wordCount ext =
    let fc = countFeatures ext
     in [ if wordCount > threshold && fc == 0
            then Just (LongNoSignal wordCount fc)
            else Nothing
        ]

checkAmbiguousPhrases :: [(Text, [Text])] -> Text -> [Maybe TriageDetail]
checkAmbiguousPhrases pairs text =
    [ let matched = filter (`containsPhrase` text) phrases
       in if null matched then Nothing else Just (AmbiguousPhrase cat matched)
    | (cat, phrases) <- pairs
    ]

checkHighVocabNoExtraction :: [(Text, [Text])] -> Int -> [Text] -> Extraction -> [Maybe TriageDetail]
checkHighVocabNoExtraction watchLists threshold wordList ext =
    [ let wordSet = Set.fromList ws
          matched = filter (\w -> Set.member (T.toLower w) wordSet) wordList
          matchCount = length matched
          existing = fromMaybe [] (Map.lookup cat (categories ext))
       in if matchCount > threshold && null existing
            then Just (HighVocabNoExtraction cat matchCount (map T.toLower matched))
            else Nothing
    | (cat, ws) <- watchLists
    ]

checkConflictingTags :: [(Text, [Text], [Text])] -> Extraction -> [Maybe TriageDetail]
checkConflictingTags pairs ext =
    [ let signals = fromMaybe [] (Map.lookup cat (categories ext))
          tagTexts = [t | TagValue t <- signals]
          pos = filter (`elem` posTags) tagTexts
          neg = filter (`elem` negTags) tagTexts
       in if not (null pos) && not (null neg)
            then Just (ConflictingTags cat pos neg)
            else Nothing
    | (cat, posTags, negTags) <- pairs
    ]

checkUnrecognizable :: Double -> Int -> Int -> [Maybe TriageDetail]
checkUnrecognizable threshold textLen nonAsciiCount =
    [ if textLen == 0
        then Nothing
        else
            let ratio = fromIntegral nonAsciiCount / fromIntegral textLen
             in if ratio > threshold
                    then Just (UnrecognizableText ratio)
                    else Nothing
    ]

-- | Case-insensitive whole-word phrase check.
containsPhrase :: Text -> Text -> Bool
containsPhrase phrase text = go (T.toLower text)
    where
        go t =
            let (before, rest) = T.breakOn phrase t
                after = T.drop (T.length phrase) rest
                prevOk = T.null before || not (isAlphaNum (T.last before))
                nextOk = T.null after || not (isAlphaNum (T.head after))
             in not (T.null rest) && ((prevOk && nextOk) || go (T.drop 1 rest))
