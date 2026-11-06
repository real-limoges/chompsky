{-# LANGUAGE OverloadedStrings #-}

-- | Shared parser type, 'ScannerEntry', and combinators used by Build.
module Chompsky.Pipeline.Parser.Internal
    ( Parser
    , ScannerEntry (..)
    , ws
    , yearP
    , optionalYear
    , oneOfLits
    , amountP
    ) where

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1, string')

import Chompsky.Types (ExtractedValue)

type Parser = Parsec Void Text

data ScannerEntry = ScannerEntry
    { seId :: Text
    {- ^ Stable identifier of the form @"<category>/<tag>"@ with a @#N@
    suffix (2-based) for duplicates within a spec.
    -}
    , seCategory :: Text
    , seParser :: Parser ExtractedValue
    }

ws :: Parser ()
ws = void space1

-- | Years 1900 through 2099 only.
yearP :: Parser Int
yearP = do
    d1 <- satisfy (\c -> c == '1' || c == '2')
    d2 <- satisfy isDigit
    d3 <- satisfy isDigit
    d4 <- satisfy isDigit
    notFollowedBy (satisfy isDigit)
    let yr = read [d1, d2, d3, d4]
    if yr >= 1900 && yr <= 2099
        then pure yr
        else fail "year out of range"

optionalYear :: Parser (Maybe Int)
optionalYear =
    optional
        ( try (ws *> string' "in" *> ws *> yearP)
            <|> try (ws *> char '(' *> yearP <* char ')')
            <|> try (ws *> yearP)
        )

{- | Match any one of the literals, case-insensitively, returning the literal
as written in the config.

A literal ending in a letter also matches with @s@ or @es@ appended, so
@"electronic device"@ matches @"electronic devices"@. Every surface form is
tried longest first, and one ending in a letter or digit must not run on
into a longer word, so @"store in"@ cannot match inside @"store information"@.
A literal ending in a space or punctuation carries its own boundary and gets
no plural forms.
-}
oneOfLits :: [Text] -> Parser Text
oneOfLits [] = fail "oneOfLits: empty list"
oneOfLits lits = choice (map litBounded (sortOn (Down . T.length . fst) (concatMap surfaceForms lits)))
    where
        surfaceForms l
            | endsWith isAlpha l = [(l <> "es", l), (l <> "s", l), (l, l)]
            | otherwise = [(l, l)]
        litBounded (surface, l) = try (l <$ string' surface <* rightBoundary surface)
        rightBoundary surface
            | endsWith isAlphaNum surface = notFollowedBy (satisfy isAlphaNum)
            | otherwise = pure ()
        endsWith p t = not (T.null t) && p (T.last t)

{- | A dollar amount: optional @$@, digits with optional thousands commas,
and an optional decimal part. A trailing sentence period (@"$25. thanks"@)
is left unconsumed rather than failing the parse.
-}
amountP :: Parser Double
amountP = do
    _ <- optional (char '$')
    intPart <- some (satisfy (\c -> isDigit c || c == ','))
    decPart <- optional (try (char '.' *> some (satisfy isDigit)))
    let intStr = filter (/= ',') intPart
        fullStr = case decPart of
            Nothing -> intStr
            Just d -> intStr ++ "." ++ d
    case reads fullStr of
        [(v, "")] -> pure v
        _ -> fail "invalid amount"
