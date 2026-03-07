-- | Membership function primitives. Exported for testing only.
module Chompsky.Pipeline.Fuzzy.Internal
    ( triangular
    , trapezoidal
    , outLow
    , outMedium
    , outHigh
    ) where

-- | Left foot @a@, peak @b@, right foot @c@.
triangular :: Double -> Double -> Double -> Double -> Double
triangular a b c x
    | x < a || x > c = 0
    | x <= b =
        if a == b
            then 1
            else (x - a) / (b - a)
    | otherwise =
        if b == c
            then 1
            else (c - x) / (c - b)

-- | Left foot @a@, left shoulder @b@, right shoulder @c@, right foot @d@.
trapezoidal :: Double -> Double -> Double -> Double -> Double -> Double
trapezoidal a b c d x
    | x < a || x > d = 0
    | x <= b = if a == b then 1 else (x - a) / (b - a)
    | x <= c = 1
    | otherwise = if c == d then 1 else (d - x) / (d - c)

-- Output confidence sets (range 0.0–1.0).
-- Exported so tests can verify the shape of the output membership functions.

outLow, outMedium, outHigh :: Double -> Double
outLow = triangular 0 0 0.4
outMedium = triangular 0.2 0.5 0.8
outHigh = triangular 0.6 1.0 1.0
