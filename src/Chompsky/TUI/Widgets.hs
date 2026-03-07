-- | Brick widgets for the TUI dashboard.
module Chompsky.TUI.Widgets
    ( progressBar
    , formatElapsed
    , formatRate
    , formatEta
    , formatPct
    ) where

import Brick (Widget)
import Brick.Widgets.Core (Padding (..), hBox, padLeft, str)
import Numeric (showFFloat)

-- | Unicode block progress bar (width in chars, excluding % suffix).
progressBar :: Int -> Int -> Int -> Widget n
progressBar width done total =
    hBox
        [ str filled
        , str empty
        , padLeft (Pad 1) (str pct)
        ]
    where
        ratio :: Double
        ratio = if total == 0 then 0 else min 1 (fromIntegral done / fromIntegral total)
        filledN = round (ratio * fromIntegral width) :: Int
        emptyN = width - filledN
        filled = replicate filledN '█'
        empty = replicate emptyN '░'
        pctVal = round (ratio * 100) :: Int
        pct = show pctVal <> "%"

formatElapsed :: Double -> String
formatElapsed secs =
    let m = floor secs `div` 60 :: Int
        s = floor secs `mod` 60 :: Int
     in pad m <> ":" <> pad s
    where
        pad n = if n < 10 then "0" <> show n else show n

-- | Returns "---" until 5+ items processed.
formatEta :: Int -> Int -> Double -> String
formatEta total processed elapsed
    | processed < 5 = "---"
    | elapsed <= 0 = "---"
    | otherwise =
        let rate = fromIntegral processed / elapsed :: Double
            remaining = fromIntegral (total - processed) / rate
         in formatElapsed remaining

formatPct :: Int -> Int -> String
formatPct n total
    | total == 0 = "0.0%"
    | otherwise =
        let pct = (fromIntegral n :: Double) / fromIntegral total * 100
         in showFFloat (Just 1) pct "%"

formatRate :: Int -> Double -> String
formatRate processed elapsed
    | elapsed <= 0 = "—"
    | otherwise =
        let rate = fromIntegral processed / elapsed :: Double
         in showFFloat (Just 0) rate "" <> " rows/sec"
