{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Strip phones, emails, repeated punctuation, and boilerplate before parsing.
module Chompsky.Pipeline.Clean
    ( clean
    ) where

import Data.Char (isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T

import Chompsky.Config (BoilerplateConfig (..))
import Chompsky.Types (CleanedText (..), EditKind (..), Span (..), TextEdit (..))

{- | Clean the text and emit an ordered edit log. Each edit's span is in the
text as it stands when that edit is applied, so replaying the log in order
over the lowercased input reproduces the cleaned text exactly. Substeps run
in this order: phone, email, repeated punctuation, whitespace, boilerplate,
trim. See docs/reference/internal/design-decisions.md ("Edit-log replay
invariant").
-}
clean :: BoilerplateConfig -> Text -> (CleanedText, [TextEdit])
clean cfg input =
    let (t1, e1) = stripPhoneNumbers (T.toLower input)
        (t2, e2) = stripEmails t1
        (t3, e3) = stripRepeatedPunctuation t2
        (t4, e4) = normalizeWhitespace t3
        (t5, e5) = stripBoilerplate cfg t4
        (t6, e6) = trimEdges t5
     in (CleanedText t6, e1 ++ e2 ++ e3 ++ e4 ++ e5 ++ e6)

{- | 'T.strip', logged: one edit for leading whitespace (applied first) and
one for trailing whitespace, each spanned in the text it applies to.
-}
trimEdges :: Text -> (Text, [TextEdit])
trimEdges t0 =
    let (lead, t1) = T.span isSpace t0
        t2 = T.dropWhileEnd isSpace t1
        trail = T.drop (T.length t2) t1
        leadEdit = [mkEdit WhitespaceCollapsed 0 lead "" | not (T.null lead)]
        trailEdit = [mkEdit WhitespaceCollapsed (T.length t2) trail "" | not (T.null trail)]
     in (t2, leadEdit ++ trailEdit)

mkEdit :: EditKind -> Int -> Text -> Text -> TextEdit
mkEdit kind pos before after =
    TextEdit
        { teKind = kind
        , teSpan = Span pos (pos + T.length before)
        , teBefore = before
        , teAfter = after
        , teDetail = Nothing
        }

{- Each substep below walks its input once, building the output in @acc@.
@pos@ is the offset in the output built so far, not in the input: it
advances by what is appended, not by what is consumed. That puts every
edit's span in the text as it stands when the edit is applied in order,
which is the replay contract on 'TextEdit'.
-}

{- | A North American number: three, three and four digits, each group
separated by nothing, a dash, a dot or a single space. The area code may sit
in parentheses, as in @"(555) 123-4567"@. The number must not sit inside a
longer run of digits on either side.
-}
stripPhoneNumbers :: Text -> (Text, [TextEdit])
stripPhoneNumbers = go 0 T.empty []
    where
        go pos acc edits t
            | T.null t = (acc, reverse edits)
            | not (afterDigit acc)
            , Just (matched, rest) <- tryParsePhone t =
                go (pos + 1) (acc <> " ") (mkEdit PhoneStripped pos matched " " : edits) rest
            | otherwise =
                go (pos + 1) (acc <> T.singleton (T.head t)) edits (T.tail t)

        afterDigit acc = not (T.null acc) && isDigit (T.last acc)

        tryParsePhone t = do
            (area, r1) <- takeAreaCode t
            (s1, r2) <- takeSep r1
            (d2, r3) <- takeDigits 3 r2
            (s2, r4) <- takeSep r3
            (d3, r5) <- takeDigits 4 r4
            case T.uncons r5 of
                Just (c, _) | isDigit c -> Nothing
                _ -> Just (area <> s1 <> d2 <> s2 <> d3, r5)

        -- Three digits, bare or in parentheses: "555" or "(555)".
        takeAreaCode t = case T.uncons t of
            Just ('(', r1) -> do
                (digits, r2) <- takeDigits 3 r1
                r3 <- T.stripPrefix ")" r2
                Just ("(" <> digits <> ")", r3)
            _ -> takeDigits 3 t

        takeDigits n t =
            let (digits, rest) = T.splitAt n t
             in if T.length digits == n && T.all isDigit digits then Just (digits, rest) else Nothing

        takeSep t = case T.uncons t of
            Just (c, rest) | c == '-' || c == '.' || c == ' ' -> Just (T.singleton c, rest)
            _ -> Just ("", t)

stripEmails :: Text -> (Text, [TextEdit])
stripEmails = go 0 T.empty []
    where
        go pos acc edits t
            | T.null t = (acc, reverse edits)
            | isSpace (T.head t) =
                let (ws, wsRest) = T.span isSpace t
                 in go (pos + T.length ws) (acc <> ws) edits wsRest
            | isEmailLike tok =
                go pos acc (mkEdit EmailMasked pos tok "" : edits) rest
            | otherwise =
                go (pos + T.length tok) (acc <> tok) edits rest
            where
                (tok, rest) = T.break isSpace t

        -- Loose heuristic: an @ somewhere, and a dot in the domain part (after the
        -- last @). Deliberately does not validate the local part.
        isEmailLike w =
            "@" `T.isInfixOf` w && "." `T.isInfixOf` T.takeWhileEnd (/= '@') w

{- | Collapse a run of two or more of @!@, @*@ and @#@ to its first character,
so @"!!!"@ becomes @"!"@. A single mark is left alone: a lone @!@ still ends a
sentence for the capture strategy's terminators.
-}
stripRepeatedPunctuation :: Text -> (Text, [TextEdit])
stripRepeatedPunctuation = go 0 T.empty []
    where
        isPunctRepeat c = c == '!' || c == '*' || c == '#'

        go pos acc edits t
            | T.null t = (acc, reverse edits)
            | isPunctRepeat (T.head t) =
                let (run, rest) = T.span isPunctRepeat t
                    kept = T.take 1 run
                 in if T.length run > 1
                        then go (pos + 1) (acc <> kept) (mkEdit PunctCollapsed pos run kept : edits) rest
                        else go (pos + 1) (acc <> run) edits rest
            | otherwise =
                go (pos + 1) (acc <> T.singleton (T.head t)) edits (T.tail t)

{- | Collapse each whitespace run to one character: a newline if the run holds
one, so line breaks survive as sentence breaks, otherwise a space.
-}
normalizeWhitespace :: Text -> (Text, [TextEdit])
normalizeWhitespace = go 0 T.empty []
    where
        go pos acc edits t
            | T.null t = (acc, reverse edits)
            | isSpace (T.head t) =
                let (run, rest) = T.span isSpace t
                    collapsed = if T.any (== '\n') run then "\n" else " "
                 in if run /= collapsed
                        then go (pos + 1) (acc <> collapsed) (mkEdit WhitespaceCollapsed pos run collapsed : edits) rest
                        else go (pos + 1) (acc <> run) edits rest
            | otherwise =
                go (pos + 1) (acc <> T.singleton (T.head t)) edits (T.tail t)

stripBoilerplate :: BoilerplateConfig -> Text -> (Text, [TextEdit])
stripBoilerplate cfg input
    | T.length input <= minPosition cfg = (input, [])
    | otherwise = foldl' step (input, []) (boilerplatePhrases cfg)
    where
        step (txt, edits) phrase = case findFrom (minPosition cfg) phrase txt of
            Nothing -> (txt, edits)
            Just pos ->
                let pLen = T.length phrase
                    stripped = T.take pos txt <> T.drop (pos + pLen) txt
                    edit = (mkEdit BoilerplateStripped pos phrase "") {teDetail = Just phrase}
                 in (stripped, edits ++ [edit])

findFrom :: Int -> Text -> Text -> Maybe Int
findFrom minPos needle haystack =
    let (before, rest) = T.breakOn needle (T.drop minPos haystack)
     in if T.null rest then Nothing else Just (minPos + T.length before)
