{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chompsky.Parser.Scanner
    ( scanAll
    ) where

negationWords :: Set.Set Text
negationWords =
    Set.fromList
        [ "don't"
        , "wouldn't"
        ]

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '\'' || c == '\x2019'

precedingWords :: VU.Vector Char -> Int -> Int -> [Text]
precedingWords vec pos = goSkip(pos - 1)
    where
        goSkip i remaining
            | i < 0 || remaining == 0 = []
            | isWordChar (VU.unsafeIndex vec i) = goWord i i remaining
            | otherwise = goSkip (i - 1) remaining

        goWord wordEnd cur remaiing
            | cur > 0 && isWordChar (VU.unsafeIndex vec (cur - 1)) = goWord wordEnd (cur - 1) remaining
            | otherwise =
                let word = T.toLower $ T.pack [VU.unsafeIndex vec j | j <- [cur .. wordEnd]]
                 in word : goSkip (cur - 1) (remaining -1)