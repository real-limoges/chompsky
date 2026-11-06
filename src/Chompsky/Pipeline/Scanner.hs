{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Run parser entries across cleaned text, collecting matches into an 'Extraction'.
module Chompsky.Pipeline.Scanner
    ( scanAll
    , scanHits
    , hitsToExtraction
    ) where

import Data.Char (isAlphaNum, isSpace)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
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
    , stateOffset
    )

import Chompsky.Pipeline.Parser.Internal (ScannerEntry (..))
import Chompsky.Types

negationWords :: Set.Set Text
negationWords =
    Set.fromList $
        concatMap
            expand
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
            ]
    where
        expand w
            | "'" `T.isInfixOf` w = [w, T.replace "'" "\x2019" w]
            | otherwise = [w]

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '\'' || c == '\x2019'

{- | Up to @n@ words immediately before @pos@, lowercased, nearest-first,
never reaching back past a sentence break. Apostrophes are word-internal so
contractions stay whole.
-}
precedingWords :: VU.Vector Char -> Int -> Int -> [Text]
precedingWords vec pos = goSkip (pos - 1)
    where
        goSkip i remaining
            | i < 0 || remaining == 0 = []
            | isWordChar (VU.unsafeIndex vec i) = goWord i i remaining
            | isSentenceBreak i = []
            | otherwise = goSkip (i - 1) remaining

        -- A newline, or sentence punctuation followed by whitespace. A decimal
        -- point, as in "25.99", has a digit after it and is not a break.
        isSentenceBreak i =
            let c = VU.unsafeIndex vec i
             in c == '\n' || (c `elem` (".;?!" :: String) && isSpace (VU.unsafeIndex vec (i + 1)))

        goWord wordEnd cur remaining
            | cur > 0 && isWordChar (VU.unsafeIndex vec (cur - 1)) = goWord wordEnd (cur - 1) remaining
            | otherwise =
                let word = T.toLower $ T.pack [VU.unsafeIndex vec j | j <- [cur .. wordEnd]]
                 in word : goSkip (cur - 1) (remaining - 1)

{- | Run every entry across the input text, returning the merged 'Extraction'
and the flat list of all hits (including negated ones, for trace consumers).
Negated hits are kept in the hit list but dropped from the 'Extraction'; see
docs/reference/internal/design-decisions.md ("Negation: scan then filter").
-}
scanAll :: [ScannerEntry] -> Text -> (Extraction, [ScanHit])
scanAll entries input =
    let hits = concat [entryHits e (collectMatches (seParser e) input) | e <- entries]
     in (hitsToExtraction hits, hits)

-- | Convenience: every hit, flat. Negated hits included.
scanHits :: [ScannerEntry] -> Text -> [ScanHit]
scanHits entries input = snd (scanAll entries input)

{- | Collapse 'ScanHit's into an 'Extraction': negated hits are dropped, and
each category keeps its distinct values, sorted.
-}
hitsToExtraction :: [ScanHit] -> Extraction
hitsToExtraction hits =
    emptyExtraction {categories = Map.map Set.toList valuesByCategory}
    where
        valuesByCategory =
            Map.fromListWith
                Set.union
                [(shCategory h, Set.singleton (shValue h)) | h <- hits, not (shIsNegated h)]

data RawMatch = RawMatch
    { rmStart :: Int
    , rmEnd :: Int
    , rmNegated :: Bool
    , rmNegWord :: Maybe Text
    , rmValue :: ExtractedValue
    }

entryHits :: ScannerEntry -> [RawMatch] -> [ScanHit]
entryHits entry =
    map $ \RawMatch {rmStart, rmEnd, rmNegated, rmNegWord, rmValue} ->
        ScanHit
            { shEntryId = seId entry
            , shCategory = seCategory entry
            , shValue = rmValue
            , shSpan = Span rmStart rmEnd
            , shIsNegated = rmNegated
            , shNegationWord = rmNegWord
            }

{- | Run one parser at every word-start position. Emits every match, including
those preceded by a negation word (the flag tells the caller).
-}
collectMatches :: Parsec Void Text ExtractedValue -> Text -> [RawMatch]
collectMatches p original = mapMaybe (\pos -> tryAt pos (V.unsafeIndex suffixVec pos)) wordStarts
    where
        originalLen = T.length original

        charVec :: VU.Vector Char
        charVec = VU.unfoldrExactN originalLen (\t -> (T.head t, T.tail t)) original

        -- O(n) total: build all suffixes by incremental T.tail
        suffixVec :: Vector Text
        suffixVec = V.unfoldrExactN originalLen (\t -> (t, T.tail t)) original

        wordStarts :: [Int]
        wordStarts = filter atWordStart [0 .. originalLen - 1]

        atWordStart 0 = True
        atWordStart i = not (isAlphaNum (VU.unsafeIndex charVec (i - 1)))

        -- Right-boundary guard: mirror the left-boundary check in 'precedingWords'
        -- so a parser can't match in the middle of a longer word.
        pBounded = p <* notFollowedBy (satisfy isAlphaNum)

        -- Drive Megaparsec at the raw 'State' level (rather than 'runParser') so we
        -- can read back the consumed length from 'stateOffset' to compute the match's
        -- end offset, starting from this word-start position's suffix.
        tryAt pos sfx =
            let (stAfter, result) = runParser' pBounded (initialState sfx)
             in case result of
                    Left _ -> Nothing
                    Right v ->
                        let negHit = find (`Set.member` negationWords) (precedingWords charVec pos 3)
                         in Just
                                RawMatch
                                    { rmStart = pos
                                    , rmEnd = pos + stateOffset stAfter
                                    , rmNegated = isJust negHit
                                    , rmNegWord = negHit
                                    , rmValue = v
                                    }

        initialState sfx =
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
