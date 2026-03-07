{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Run parser entries across cleaned text, collecting matches into an 'Extraction'.
module Chompsky.Pipeline.Scanner
    ( scanAll
    ) where

import Data.Char (isAlphaNum)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Void (Void)
import Text.Megaparsec
    ( Parsec
    , PosState (..)
    , State (..)
    , defaultTabWidth
    , initialPos
    , notFollowedBy
    , runParser'
    , satisfy
    )

import Chompsky.Pipeline.Parser.Internal (ScannerEntry (..))
import Chompsky.Types

{- | Words that negate the feature described by what follows them.
Includes contractions in both ASCII-apostrophe and Unicode smart-quote forms.
-}
negationWords :: Set.Set Text
negationWords =
    Set.fromList
        [ "no"
        , "not"
        , "never"
        , "without"
        , "neither"
        , "nor"
        , "don't"
        , "doesn't"
        , "didn't"
        , "won't"
        , "wouldn't"
        , "can't"
        , "couldn't"
        , "shouldn't"
        , "isn't"
        , "aren't"
        , "wasn't"
        , "weren't"
        , "hasn't"
        , "haven't"
        , "hadn't"
        , "don\x2019t"
        , "doesn\x2019t"
        , "didn\x2019t"
        , "won\x2019t"
        , "wouldn\x2019t"
        , "can\x2019t"
        , "couldn\x2019t"
        , "shouldn\x2019t"
        , "isn\x2019t"
        , "aren\x2019t"
        , "wasn\x2019t"
        , "weren\x2019t"
        , "hasn\x2019t"
        , "haven\x2019t"
        , "hadn\x2019t"
        ]

{- | Alphanumeric or apostrophe — used to keep contractions intact when
walking backward through the character vector.  Do NOT use for 'atWordStart'.
-}
isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '\'' || c == '\x2019'

{- | Extract up to @n@ words immediately before position @pos@ in @vec@,
walking backward through the character vector.  Words are lowercased and
returned nearest-first.  Apostrophes are treated as word-internal so that
contractions like "doesn't" are kept whole.
-}
precedingWords :: VU.Vector Char -> Int -> Int -> [Text]
precedingWords vec pos = goSkip (pos - 1)
    where
        goSkip i remaining
            | i < 0 || remaining == 0 = []
            | isWordChar (VU.unsafeIndex vec i) = goWord i i remaining
            | otherwise = goSkip (i - 1) remaining

        goWord wordEnd cur remaining
            | cur > 0 && isWordChar (VU.unsafeIndex vec (cur - 1)) = goWord wordEnd (cur - 1) remaining
            | otherwise =
                let word = T.toLower $ T.pack [VU.unsafeIndex vec j | j <- [cur .. wordEnd]]
                 in word : goSkip (cur - 1) (remaining - 1)

scanAll :: [ScannerEntry] -> Text -> Extraction
scanAll entries input = foldl' applyEntry emptyExtraction entries
    where
        applyEntry ext entry =
            let results = collectMatches (seParser entry) input
                cat = seCategory entry
                existing = fromMaybe [] (Map.lookup cat (categories ext))
                merged = case seMerge entry of
                    CollectUnique ->
                        Set.toList $ Set.fromList (existing ++ results)
                    FirstWins ->
                        if null existing then take 1 results else existing
                    Concatenate ->
                        existing ++ results
             in if null merged
                    then ext
                    else ext {categories = Map.insert cat merged (categories ext)}

-- | Run one parser at every word-start position; deduplication is the caller's job.
collectMatches :: Parsec Void Text ExtractedValue -> Text -> [ExtractedValue]
collectMatches p original = go wordStarts
    where
        originalLen = T.length original

        charVec :: VU.Vector Char
        charVec = VU.unfoldrExactN originalLen (\t -> (T.head t, T.tail t)) original

        -- O(n) total: build all suffixes by incremental T.tail
        suffixVec :: Vector Text
        suffixVec = V.unfoldrExactN originalLen (\t -> (t, T.tail t)) original

        -- Precompute word-start positions so we skip non-starts entirely
        wordStarts :: [Int]
        wordStarts = [pos | pos <- [0 .. originalLen - 1], atWordStart pos]

        atWordStart :: Int -> Bool
        atWordStart 0 = True
        atWordStart i = not (isAlphaNum (VU.unsafeIndex charVec (i - 1)))

        go :: [Int] -> [ExtractedValue]
        go [] = []
        go (pos : rest) =
            -- SAFETY: pos is drawn from wordStarts which filters [0..originalLen-1],
            -- so pos is always a valid index into suffixVec (length originalLen).
            case tryAt (V.unsafeIndex suffixVec pos) of
                Just x ->
                    if any (`Set.member` negationWords) (precedingWords charVec pos 3)
                        then go rest
                        else x : go rest
                Nothing -> go rest

        -- \| Try the parser at this suffix; enforce word boundary after match.
        tryAt :: Text -> Maybe ExtractedValue
        tryAt sfx =
            let pBounded = p <* notFollowedBy (satisfy isAlphaNum)
                st =
                    State
                        { stateInput = sfx
                        , stateOffset = 0
                        , statePosState =
                            PosState
                                { pstateInput = sfx
                                , pstateOffset = 0
                                , pstateSourcePos = initialPos "<remark>"
                                , pstateTabWidth = defaultTabWidth
                                , pstateLinePrefix = ""
                                }
                        , stateParseErrors = []
                        }
                (_, result) = runParser' pBounded st
             in case result of
                    Right v -> Just v
                    Left _ -> Nothing
