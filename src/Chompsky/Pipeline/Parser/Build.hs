{-# LANGUAGE OverloadedStrings #-}

-- | Build 'ScannerEntry' values from Lua-driven 'ParserSpec' definitions.
module Chompsky.Pipeline.Parser.Build
    ( buildEntries
    ) where

import Control.Applicative (many)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (choice, optional, satisfy, try)

import Chompsky.Config.ParserSpec
    ( ParserSpec (..)
    , ParserStrategy (..)
    , SpecEntry (..)
    )
import Chompsky.Pipeline.Parser.Internal
import Chompsky.Types

buildEntries :: ParserSpec -> [ScannerEntry]
buildEntries spec = go Map.empty (psEntries spec)
    where
        cat = psCategory spec
        go _ [] = []
        go seen (e : es) =
            let base = cat <> "/" <> seTag e
                count = Map.findWithDefault (0 :: Int) base seen
                eid = if count == 0 then base else base <> "#" <> T.pack (show (count + 1))
                seen' = Map.insert base (count + 1) seen
             in buildOne spec eid e : go seen' es

buildOne :: ParserSpec -> Text -> SpecEntry -> ScannerEntry
buildOne spec eid entry =
    let cat = psCategory spec
        merge = CollectUnique
        parser = case psStrategy spec of
            PhraseStrategy -> buildPhraseParser entry
            VerbalStrategy -> buildVerbalParser spec entry
            TriggerStrategy -> buildTriggerParser entry
            MonetaryStrategy -> buildMonetaryParser entry
            CaptureStrategy -> buildCaptureParser entry
     in ScannerEntry eid cat merge parser

buildPhraseParser :: SpecEntry -> Parser ExtractedValue
buildPhraseParser entry =
    TagValue (seTag entry) <$ oneOfLits (sePhrases entry)

buildVerbalParser :: ParserSpec -> SpecEntry -> Parser ExtractedValue
buildVerbalParser spec entry =
    choice
        [ try prefixForm
        , try suffixForm
        , try adverbDirectForm
        ]
    where
        tag = seTag entry
        synonyms = seSynonyms entry
        suffixExtra = seSuffixExtra entry
        adverbs = psAdverbs spec
        actionVerbs = psActionVerbs spec
        suffixVerbs = psSuffixVerbs spec
        directPhrases = seDirectPhrases entry

        -- "replaced roof" / "updated kitchen" — verb then synonym
        prefixForm = do
            _ <- oneOfLits actionVerbs
            ws
            _ <- optional (oneOfLits adverbs <* ws)
            _ <- oneOfLits synonyms
            TagWithYear tag <$> optionalYear

        -- "roof replaced" / "kitchen updated in 2020" — synonym then verb
        suffixForm = do
            _ <- oneOfLits (synonyms ++ suffixExtra)
            ws
            _ <- optional (oneOfLits adverbs <* ws)
            _ <- oneOfLits suffixVerbs
            TagWithYear tag <$> optionalYear

        -- "recently installed solar" — adverb then direct phrase
        adverbDirectForm = do
            _ <- oneOfLits adverbs
            ws
            _ <- oneOfLits (if null directPhrases then synonyms else directPhrases)
            TagWithYear tag <$> optionalYear

buildTriggerParser :: SpecEntry -> Parser ExtractedValue
buildTriggerParser entry = do
    _ <- oneOfLits (seTriggers entry)
    _ <- optional ws
    YearValue <$> yearP

buildMonetaryParser :: SpecEntry -> Parser ExtractedValue
buildMonetaryParser entry = do
    _ <- oneOfLits (seLabels entry)
    _ <- optional (ws *> oneOfLits (seBridges entry))
    _ <- optional ws
    amt <- amountP
    _ <- optional (ws *> oneOfLits (seFrequency entry))
    pure (AmountValue amt)

buildCaptureParser :: SpecEntry -> Parser ExtractedValue
buildCaptureParser entry = do
    _ <- oneOfLits (seTriggers entry)
    _ <- optional ws
    rest <- many (satisfy (not . isTerminator))
    let captured = T.strip (T.pack rest)
    if T.null captured
        then fail "empty capture"
        else pure (CapturedText captured)
    where
        terminators = T.unpack (seTerminators entry)
        isTerminator c
            | null terminators = c == '.' || c == '!' || c == ';'
            | otherwise = c `elem` terminators
