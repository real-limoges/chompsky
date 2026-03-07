{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Strip phones, emails, repeated punctuation, and boilerplate before parsing.
module Chompsky.Pipeline.Clean
    ( clean
    ) where

import Data.Char (isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B

import Chompsky.Config (BoilerplateConfig (..))
import Chompsky.Types (CleanedText (..))

clean :: BoilerplateConfig -> Text -> CleanedText
clean cfg input =
    let lowered = T.toLower input
        noPhones = stripPhoneNumbers lowered
        noEmails = stripEmails noPhones
        noPunct = stripRepeatedPunctuation noEmails
        normalized = normalizeWhitespace noPunct
        (stripped, _) = stripBoilerplate cfg normalized
        trimmed = T.strip stripped
     in CleanedText trimmed

stripPhoneNumbers :: Text -> Text
stripPhoneNumbers t = TL.toStrict . B.toLazyText $ go t
    where
        go t'
            | T.null t' = mempty
            | otherwise =
                case tryParsePhone t' of
                    Just (_, rest) -> B.singleton ' ' <> go rest
                    Nothing ->
                        let (c, rest) = (T.head t', T.tail t')
                         in B.singleton c <> go rest

        tryParsePhone t' = do
            (d1, r1) <- takeDigits 3 t'
            (_, r2) <- takeSep r1
            (d2, r3) <- takeDigits 3 r2
            (_, r4) <- takeSep r3
            (d3, r5) <- takeDigits 4 r4
            -- Make sure next char is not a digit (word boundary)
            case T.uncons r5 of
                Just (c, _) | isDigit c -> Nothing
                _ -> Just (d1 <> d2 <> d3, r5)

        takeDigits n t' =
            let (digits, rest) = T.span isDigit t'
             in if T.length digits == n then Just (digits, rest) else Nothing

        takeSep t' = case T.uncons t' of
            Just (c, rest) | c == '-' || c == '.' -> Just (T.singleton c, rest)
            _ -> Just ("", t') -- separator is optional

stripEmails :: Text -> Text
stripEmails = T.unwords . map maskEmail . T.words
    where
        maskEmail w
            | "@" `T.isInfixOf` w && "." `T.isInfixOf` T.takeWhileEnd (/= '@') w = ""
            | otherwise = w

stripRepeatedPunctuation :: Text -> Text
stripRepeatedPunctuation t = TL.toStrict . B.toLazyText $ go False t
    where
        isPunctRepeat c = c == '!' || c == '*' || c == '#'
        go _ t' | T.null t' = mempty
        go seenPunct t' =
            let (c, rest) = (T.head t', T.tail t')
             in if isPunctRepeat c
                    then
                        if seenPunct
                            then go True rest
                            else B.singleton ' ' <> go True rest
                    else B.singleton c <> go False rest

normalizeWhitespace :: Text -> Text
normalizeWhitespace = T.intercalate " " . filter (not . T.null) . T.split isSpace

-- minPosition prevents us from truncating short remarks that happen to
-- contain boilerplate-like phrases. If the entire remark is shorter than
-- the threshold, it's probably real content, not a disclaimer.
stripBoilerplate :: BoilerplateConfig -> Text -> (Text, [Text])
stripBoilerplate cfg input
    | T.length input <= minPosition cfg = (input, [])
    | otherwise = findEarliestBoilerplate (boilerplatePhrases cfg) input (minPosition cfg)

findEarliestBoilerplate :: [Text] -> Text -> Int -> (Text, [Text])
findEarliestBoilerplate phrases input minPos = (T.strip result, removed)
    where
        (result, removed) = foldl' removePhrase (input, []) phrases

        removePhrase (txt, acc) phrase =
            case findFrom minPos phrase txt of
                Nothing -> (txt, acc)
                Just pos ->
                    let before = T.take pos txt
                        after = T.drop (pos + T.length phrase) txt
                     in (before <> after, acc <> [phrase])

findFrom :: Int -> Text -> Text -> Maybe Int
findFrom minPos needle haystack =
    let sub = T.drop minPos haystack
     in case T.breakOn needle sub of
            (_, rest) | T.null rest -> Nothing
            (before, _) -> Just (minPos + T.length before)
