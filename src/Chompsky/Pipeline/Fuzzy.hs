{-# LANGUAGE OverloadedStrings #-}

-- | Mamdani fuzzy inference: feature count + ambiguity → confidence score and tier.
module Chompsky.Pipeline.Fuzzy
    ( fuzzyConfidence
    ) where

import Chompsky.Types (Confidence (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Hazy

{- | Inputs clamped: featureCount ≤ 10, ambiguityCount ≤ 5.
Tiers: High ≥ 0.65, Medium ≥ 0.35, Low < 0.35.
-}
fuzzyConfidence :: Int -> Int -> (Double, Confidence)
fuzzyConfidence featureCount ambiguityCount =
    let inputs =
            Map.fromList
                [ ("features", min 10.0 (fromIntegral featureCount))
                , ("ambiguity", min 5.0 (fromIntegral ambiguityCount))
                ]
        outputs = evaluate confidenceFIS inputs
        score = Map.findWithDefault 0.5 "confidence" outputs
        tier
            | score >= 0.65 = High
            | score >= 0.35 = Medium
            | otherwise = Low
     in (score, tier)

confidenceFIS :: FIS
confidenceFIS =
    FIS
        { fisName = "confidence"
        , fisInputs =
            Map.fromList
                [ ("features", featuresVar)
                , ("ambiguity", ambiguityVar)
                ]
        , fisOutputs = Map.singleton "confidence" confidenceVar
        , fisRules = rules
        , fisMethod = Mamdani
        }

featuresVar :: LinguisticVar
featuresVar =
    LinguisticVar
        { lvName = "features"
        , lvTerms =
            Map.fromList
                [ ("few", FuzzySet "few" (triangular 0 0 2) (0, 10))
                , ("some", FuzzySet "some" (triangular 1 3 5) (0, 10))
                , ("many", FuzzySet "many" (trapezoidal 4 6 10 10) (0, 10))
                ]
        , lvBounds = (0, 10)
        }

ambiguityVar :: LinguisticVar
ambiguityVar =
    LinguisticVar
        { lvName = "ambiguity"
        , lvTerms =
            Map.fromList
                [ ("none", FuzzySet "none" (triangular 0 0 1) (0, 5))
                , ("mild", FuzzySet "mild" (triangular 0 1 3) (0, 5))
                , ("severe", FuzzySet "severe" (trapezoidal 2 3 5 5) (0, 5))
                ]
        , lvBounds = (0, 5)
        }

confidenceVar :: LinguisticVar
confidenceVar =
    LinguisticVar
        { lvName = "confidence"
        , lvTerms =
            Map.fromList
                [ ("low", FuzzySet "low" (triangular 0 0 0.4) (0, 1))
                , ("medium", FuzzySet "medium" (triangular 0.2 0.5 0.8) (0, 1))
                , ("high", FuzzySet "high" (triangular 0.6 1.0 1.0) (0, 1))
                ]
        , lvBounds = (0, 1)
        }

rules :: [FuzzyRule]
rules =
    [ rule ["features" ~> "few", "ambiguity" ~> "none"] "low"
    , rule ["features" ~> "few", "ambiguity" ~> "mild"] "low"
    , rule ["features" ~> "few", "ambiguity" ~> "severe"] "low"
    , rule ["features" ~> "some", "ambiguity" ~> "none"] "high"
    , rule ["features" ~> "some", "ambiguity" ~> "mild"] "medium"
    , rule ["features" ~> "some", "ambiguity" ~> "severe"] "low"
    , rule ["features" ~> "many", "ambiguity" ~> "none"] "high"
    , rule ["features" ~> "many", "ambiguity" ~> "mild"] "medium"
    , rule ["features" ~> "many", "ambiguity" ~> "severe"] "low"
    ]

(~>) :: T.Text -> T.Text -> (T.Text, T.Text)
(~>) = (,)

rule :: [(T.Text, T.Text)] -> T.Text -> FuzzyRule
rule antecedents consequentTerm =
    FuzzyRule
        { ruleAntecedent = antecedents
        , ruleConsequent = [("confidence", consequentTerm)]
        }
