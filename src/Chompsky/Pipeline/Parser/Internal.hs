{-# LANGUAGE OverloadedStrings #-}

-- | Shared parser type, 'ScannerEntry', and combinators used by Build.
module Chompsky.Pipeline.Parser.Internal
    ( Parser
    , ScannerEntry (..)
    , lit
    , ws
    , yearP
    , optionalYear
    , oneOfLits
    , amountP
    ) where

import Control.Monad (void)
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1, string')

import Chompsky.Types (ExtractedValue, MergeStrategy)

type Parser = Parsec Void Text

data ScannerEntry = ScannerEntry
    { seCategory :: Text
    , seMerge :: MergeStrategy
    , seParser :: Parser ExtractedValue
    }

lit :: Text -> Parser Text
lit = string'

ws :: Parser ()
ws = void space1

-- | 1900–2099 only.
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
        ( try (ws *> lit "in" *> ws *> yearP)
            <|> try (ws *> char '(' *> yearP <* char ')')
            <|> try (ws *> yearP)
        )

oneOfLits :: [Text] -> Parser Text
oneOfLits [] = fail "oneOfLits: empty list"
oneOfLits lits = choice (map (\l -> l <$ lit l) lits)

amountP :: Parser Double
amountP = do
    _ <- optional (char '$')
    intPart <- some (satisfy (\c -> isDigit c || c == ','))
    decPart <- optional (char '.' *> some (satisfy isDigit))
    let intStr = filter (/= ',') intPart
        fullStr = case decPart of
            Nothing -> intStr
            Just d -> intStr ++ "." ++ d
    case reads fullStr of
        [(v, "")] -> pure v
        _ -> fail "invalid amount"
