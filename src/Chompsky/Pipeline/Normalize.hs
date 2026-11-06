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
input. See docs/reference/internal/design-decisions.md ("Edit-log replay
invariant").
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

{- | Boundary-checked global replace. @outPos@ tracks the offset in the text
being built (for edit spans) while @remaining@ is the input still to consume;
the two diverge whenever @rep@ and @pat@ differ in length.
-}
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

{- | Replace whole words from the dictionary. @outPos@ is the offset in the
output built so far, so each edit's span is in the text as it stands when
that edit is applied in order (the same contract as 'replacePhraseAll').
-}
applyWordReplacements :: HashMap Text Text -> Text -> (Text, [TextEdit])
applyWordReplacements dict input =
    let (pieces, edits, _) = foldl' step ([], [], 0) (tokenize input)
     in (T.concat (reverse pieces), reverse edits)
    where
        step (pieces, edits, outPos) (Word w)
            | Just rep <- HashMap.lookup w dict =
                let ed =
                        TextEdit
                            { teKind = AbbrevWord
                            , teSpan = Span outPos (outPos + T.length w)
                            , teBefore = w
                            , teAfter = rep
                            , teDetail = Just w
                            }
                 in (rep : pieces, ed : edits, outPos + T.length rep)
            | otherwise = (w : pieces, edits, outPos + T.length w)
        step (pieces, edits, outPos) (Sep s) = (s : pieces, edits, outPos + T.length s)

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
