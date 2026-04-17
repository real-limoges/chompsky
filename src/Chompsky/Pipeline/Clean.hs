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
text at the moment that substep ran. Substeps run in this order: phone,
email, repeated punctuation, whitespace, boilerplate, trim.
-}
clean :: BoilerplateConfig -> Text -> (CleanedText, [TextEdit])
clean cfg input =
    let (t1, e1) = stripPhoneNumbers (T.toLower input)
        (t2, e2) = stripEmails t1
        (t3, e3) = stripRepeatedPunctuation t2
        (t4, e4) = normalizeWhitespace t3
        (t5, e5) = stripBoilerplate cfg t4
     in (CleanedText (T.strip t5), e1 ++ e2 ++ e3 ++ e4 ++ e5)

mkEdit :: EditKind -> Int -> Text -> Text -> TextEdit
mkEdit kind pos before after =
    TextEdit
        { teKind = kind
        , teSpan = Span pos (pos + T.length before)
        , teBefore = before
        , teAfter = after
        , teDetail = Nothing
        }

stripPhoneNumbers :: Text -> (Text, [TextEdit])
stripPhoneNumbers = go 0 T.empty []
    where
        go pos acc edits t
            | T.null t = (acc, reverse edits)
            | Just (matched, rest) <- tryParsePhone t =
                let mLen = T.length matched
                 in go (pos + mLen) (acc <> " ") (mkEdit PhoneStripped pos matched " " : edits) rest
            | otherwise =
                go (pos + 1) (acc <> T.singleton (T.head t)) edits (T.tail t)

        tryParsePhone t = do
            (d1, r1) <- takeDigits 3 t
            (s1, r2) <- takeSep r1
            (d2, r3) <- takeDigits 3 r2
            (s2, r4) <- takeSep r3
            (d3, r5) <- takeDigits 4 r4
            case T.uncons r5 of
                Just (c, _) | isDigit c -> Nothing
                _ -> Just (d1 <> s1 <> d2 <> s2 <> d3, r5)

        takeDigits n t =
            let (digits, rest) = T.span isDigit t
             in if T.length digits == n then Just (digits, rest) else Nothing

        takeSep t = case T.uncons t of
            Just (c, rest) | c == '-' || c == '.' -> Just (T.singleton c, rest)
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
                go (pos + T.length tok) acc (mkEdit EmailMasked pos tok "" : edits) rest
            | otherwise =
                go (pos + T.length tok) (acc <> tok) edits rest
            where
                (tok, rest) = T.break isSpace t

        isEmailLike w =
            "@" `T.isInfixOf` w && "." `T.isInfixOf` T.takeWhileEnd (/= '@') w

stripRepeatedPunctuation :: Text -> (Text, [TextEdit])
stripRepeatedPunctuation = go 0 T.empty []
    where
        isPunctRepeat c = c == '!' || c == '*' || c == '#'

        go pos acc edits t
            | T.null t = (acc, reverse edits)
            | isPunctRepeat (T.head t) =
                let (run, rest) = T.span isPunctRepeat t
                 in go (pos + T.length run) (acc <> " ") (mkEdit PunctCollapsed pos run " " : edits) rest
            | otherwise =
                go (pos + 1) (acc <> T.singleton (T.head t)) edits (T.tail t)

normalizeWhitespace :: Text -> (Text, [TextEdit])
normalizeWhitespace = go 0 T.empty []
    where
        go pos acc edits t
            | T.null t = (acc, reverse edits)
            | isSpace (T.head t) =
                let (run, rest) = T.span isSpace t
                    runLen = T.length run
                    changed = runLen > 1 || run /= " "
                 in if changed
                        then go (pos + runLen) (acc <> " ") (mkEdit WhitespaceCollapsed pos run " " : edits) rest
                        else go (pos + runLen) (acc <> run) edits rest
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
