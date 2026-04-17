-- | Expand abbreviations via phrase then word replacement with word-boundary guards.
module Chompsky.Pipeline.Normalize
    ( normalize
    ) where

import Data.Char (isAlphaNum)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as T

import Chompsky.Config (AbbreviationConfig (..))
import Chompsky.Types (EditKind (..), Span (..), TextEdit (..))

{- | Run both phases; each edit's span references the text at the moment that
edit fires, so consumers can replay edits in order against the lowercased
input.
-}
normalize :: AbbreviationConfig -> Text -> (Text, [TextEdit])
normalize cfg input =
    let (afterPhrases, pe) = applyPhraseReplacements (phraseReplacements cfg) (T.toLower input)
        (afterWords, we) = applyWordReplacements (wordReplacements cfg) afterPhrases
     in (afterWords, pe ++ we)

applyPhraseReplacements :: [(Text, Text)] -> Text -> (Text, [TextEdit])
applyPhraseReplacements pairs t0 =
    foldl' step (t0, []) pairs
    where
        step (txt, edits) (pat, rep) =
            let (txt', new) = replacePhraseAll pat rep txt
             in (txt', edits ++ new)

replacePhraseAll :: Text -> Text -> Text -> (Text, [TextEdit])
replacePhraseAll pat rep = go 0 T.empty
    where
        patLen = T.length pat
        repLen = T.length rep

        go outPos acc remaining
            | T.null match = (acc <> remaining, [])
            | isWordBoundary acc before after =
                let matchPos = outPos + T.length before
                    ed = mkEdit matchPos
                    (final, rest) = go (matchPos + repLen) (acc <> before <> rep) after
                 in (final, ed : rest)
            | otherwise =
                let (h, t) = T.splitAt 1 match
                 in go (outPos + T.length before + 1) (acc <> before <> h) t
            where
                (before, match) = T.breakOn pat remaining
                after = T.drop patLen match

        mkEdit pos =
            TextEdit
                { teKind = AbbrevPhrase
                , teSpan = Span pos (pos + patLen)
                , teBefore = pat
                , teAfter = rep
                , teDetail = Just pat
                }

        isWordBoundary acc before after =
            let leftChar
                    | not (T.null before) = Just (T.last before)
                    | not (T.null acc) = Just (T.last acc)
                    | otherwise = Nothing
                leftOk = maybe True (not . isAlphaNum) leftChar
                rightOk = T.null after || not (isAlphaNum (T.head after))
             in leftOk && rightOk

applyWordReplacements :: HashMap Text Text -> Text -> (Text, [TextEdit])
applyWordReplacements dict input =
    let (pieces, edits) = foldl' step ([], []) (zipWithOffsets (tokenize input))
     in (T.concat (reverse pieces), reverse edits)
    where
        step (pieces, edits) (off, Word w)
            | Just rep <- HashMap.lookup w dict =
                let ed =
                        TextEdit
                            { teKind = AbbrevWord
                            , teSpan = Span off (off + T.length w)
                            , teBefore = w
                            , teAfter = rep
                            , teDetail = Just w
                            }
                 in (rep : pieces, ed : edits)
            | otherwise = (w : pieces, edits)
        step (pieces, edits) (_, Sep s) = (s : pieces, edits)

zipWithOffsets :: [Token] -> [(Int, Token)]
zipWithOffsets = go 0
    where
        go _ [] = []
        go n (t : ts) = (n, t) : go (n + tokenLen t) ts
        tokenLen (Word w) = T.length w
        tokenLen (Sep s) = T.length s

data Token = Word Text | Sep Text

-- | @/@ is treated as part of a word so abbreviations like @"a/c"@ stay together.
tokenize :: Text -> [Token]
tokenize t
    | T.null t = []
    | isWordStart (T.head t) =
        let (word, rest) = T.span isWordChar t
         in Word word : tokenize rest
    | otherwise =
        let (sep, rest) = T.span (not . isWordChar) t
         in Sep sep : tokenize rest
    where
        isWordChar c = isAlphaNum c || c == '/'
        isWordStart = isWordChar
