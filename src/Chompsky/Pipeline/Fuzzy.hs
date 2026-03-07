-- | Mamdani fuzzy inference: feature count + ambiguity → confidence score and tier.
module Chompsky.Pipeline.Fuzzy
    ( fuzzyConfidence
    ) where

import Chompsky.Pipeline.Fuzzy.Internal (trapezoidal, triangular)
import Chompsky.Types (Confidence (..))

{- | Inputs clamped: featureCount ≤ 10, ambiguityCount ≤ 5.
Tiers: High ≥ 0.65, Medium ≥ 0.35, Low < 0.35.
-}
fuzzyConfidence :: Int -> Int -> (Double, Confidence)
fuzzyConfidence featureCount ambiguityCount =
    let features = min 10.0 (fromIntegral featureCount)
        ambiguity = min 5.0 (fromIntegral ambiguityCount)
        (wLow, wMed, wHigh) = applyRules features ambiguity
        score = defuzzify wLow wMed wHigh
        tier
            | score >= 0.65 = High
            | score >= 0.35 = Medium
            | otherwise = Low
     in (score, tier)

-- Feature richness (range 0–10+)
featFew, featSome, featMany :: Double -> Double
featFew = triangular 0 0 2
featSome = triangular 1 3 5
featMany = trapezoidal 4 6 10 10

-- Ambiguity severity (range 0–5)
ambNone, ambMild, ambSevere :: Double -> Double
ambNone = triangular 0 0 1
ambMild = triangular 0 1 3
ambSevere = trapezoidal 2 3 5 5

-- Pre-computed centroids of the output sets (analytical for symmetric triangles).
centroidLow, centroidMedium, centroidHigh :: Double
centroidLow = (0.0 + 0.0 + 0.4) / 3.0 -- ≈ 0.133
centroidMedium = (0.2 + 0.5 + 0.8) / 3.0 -- 0.5
centroidHigh = (0.6 + 1.0 + 1.0) / 3.0 -- ≈ 0.867

-- | 9-rule Mamdani rule base; each rule fires with strength = min(antecedents).
applyRules :: Double -> Double -> (Double, Double, Double)
applyRules features ambiguity =
    let ff = featFew features
        fs = featSome features
        fm = featMany features
        an = ambNone ambiguity
        am = ambMild ambiguity
        as_ = ambSevere ambiguity
        r1 = min ff an -- few ∧ none  → low
        r2 = min ff am -- few ∧ mild  → low
        r3 = min ff as_ -- few ∧ severe → low
        r4 = min fs an -- some ∧ none  → high
        r5 = min fs am -- some ∧ mild  → medium
        r6 = min fs as_ -- some ∧ severe → low
        r7 = min fm an -- many ∧ none  → high
        r8 = min fm am -- many ∧ mild  → medium
        r9 = min fm as_ -- many ∧ severe → low
        activationLow = maximum [r1, r2, r3, r6, r9]
        activationMedium = max r5 r8
        activationHigh = max r4 r7
     in (activationLow, activationMedium, activationHigh)

-- Converts the fuzzy output back to a single crisp number via weighted average
-- of each output set's centroid, where weights are the rule activation strengths.
defuzzify :: Double -> Double -> Double -> Double
defuzzify wLow wMed wHigh
    | totalWeight == 0 = 0.5 -- degenerate case: no rule fired at all
    | otherwise = numerator / totalWeight
    where
        numerator = wLow * centroidLow + wMed * centroidMedium + wHigh * centroidHigh
        totalWeight = wLow + wMed + wHigh
