-- | Expand abbreviations via phrase then word replacement with word-boundary guards.
module Chompsky.Pipeline.Normalize
    ( normalize
    ) where

import Data.Char (isAlphaNum)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B

import Chompsky.Config (AbbreviationConfig (..))

normalize :: AbbreviationConfig -> Text -> Text
normalize cfg input =
    let lowered = T.toLower input
        afterPhrases = applyPhraseReplacements (phraseReplacements cfg) lowered
        afterWords = applyWordReplacements (wordReplacements cfg) afterPhrases
     in afterWords

-- | Pairs are pre-sorted longest-first so longer patterns get priority.
applyPhraseReplacements :: [(Text, Text)] -> Text -> Text
applyPhraseReplacements pairs t =
    foldl' (\acc (pat, rep) -> replacePhraseAll pat rep acc) t pairs

-- | Replace all occurrences with word-boundary enforcement.
replacePhraseAll :: Text -> Text -> Text -> Text
replacePhraseAll pat rep t = TL.toStrict . B.toLazyText $ go t
    where
        patLen = T.length pat
        go t' =
            let (before, match) = T.breakOn pat t'
             in if T.null match
                    then B.fromText t'
                    else
                        let after = T.drop patLen match

                            boundaryBefore =
                                T.null before
                                    || not (isAlphaNum (T.last before))

                            boundaryAfter =
                                T.null after
                                    || not (isAlphaNum (T.head after))
                         in if boundaryBefore && boundaryAfter
                                then B.fromText before <> B.fromText rep <> go after
                                else B.fromText before <> B.singleton (T.head match) <> go (T.drop 1 match)

applyWordReplacements :: HashMap Text Text -> Text -> Text
applyWordReplacements dict input =
    let tokens = tokenize input
        replaced = map (replaceToken dict) tokens
     in T.concat replaced

data Token = Word Text | Sep Text
    deriving (Show)

-- | @/@ is treated as part of a word so abbreviations like @"a/c"@ stay together.
tokenize :: Text -> [Token]
tokenize t
    | T.null t = []
    | isAlphaNum (T.head t) || T.head t == '/' =
        let (word, rest) = T.span (\c -> isAlphaNum c || c == '/') t
         in Word word : tokenize rest
    | otherwise =
        let (sep, rest) = T.span (\c -> not (isAlphaNum c) && c /= '/') t
         in Sep sep : tokenize rest

replaceToken :: HashMap Text Text -> Token -> Text
replaceToken dict (Word w) = fromMaybe w (HashMap.lookup w dict)
replaceToken _ (Sep s) = s
