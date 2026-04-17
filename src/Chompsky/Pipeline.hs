{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

-- | Top-level pipeline composition: normalize, clean, scan, triage. Pure -- no IO.
module Chompsky.Pipeline
    ( processRemarkPure
    , processRemarkPureWith
    , processRemarkTraced
    , processRemarkTracedWith
    , ProcessResult (..)
    ) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)

import Chompsky.Config
    ( AbbreviationConfig
    , AppConfig (..)
    , BoilerplateConfig
    , abbreviationConfig
    , boilerplateConfig
    , parserSpecs
    )
import Chompsky.Pipeline.Clean (clean)
import Chompsky.Pipeline.Normalize (normalize)
import Chompsky.Pipeline.Parser (ScannerEntry, allEntries)
import Chompsky.Pipeline.Scanner (scanAll)
import Chompsky.Pipeline.Triage (triage)
import Chompsky.Types
    ( CleanedText (..)
    , Extraction (..)
    , FuzzySnapshot (..)
    , ProcessTrace (..)
    , TriageConfig
    )

-- | The result of running the full pipeline on a single input text.
data ProcessResult = ProcessResult
    { prExtraction :: Extraction
    -- ^ The extraction produced by scanning and triage.
    , prCleanedText :: CleanedText
    -- ^ The cleaned text that was fed to the scanner.
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Rebuilds scanner entries on every call; use 'processRemarkPureWith' for batch.
processRemarkPure :: AppConfig -> Text -> ProcessResult
processRemarkPure cfg =
    processRemarkPureWith
        (triageConfig cfg)
        (abbreviationConfig cfg)
        (boilerplateConfig cfg)
        (allEntries (parserSpecs cfg))

-- | Run the pipeline with pre-built scanner entries (for batch loops).
processRemarkPureWith ::
    TriageConfig -> AbbreviationConfig -> BoilerplateConfig -> [ScannerEntry] -> Text -> ProcessResult
processRemarkPureWith triageCfg abbrevCfg bpCfg entries rawRemarks =
    let (normalized, _) = normalize abbrevCfg rawRemarks
        (cleaned, _) = clean bpCfg normalized
        (extraction, _) = scanAll entries (unCleanedText cleaned)
        triaged = triage triageCfg cleaned extraction
     in ProcessResult triaged cleaned

-- | Like 'processRemarkPure' but captures every stage's output for trace consumers.
processRemarkTraced :: AppConfig -> Text -> ProcessTrace
processRemarkTraced cfg =
    processRemarkTracedWith
        (triageConfig cfg)
        (abbreviationConfig cfg)
        (boilerplateConfig cfg)
        (allEntries (parserSpecs cfg))

-- | Traced pipeline with pre-built scanner entries (for batch trace generation).
processRemarkTracedWith ::
    TriageConfig -> AbbreviationConfig -> BoilerplateConfig -> [ScannerEntry] -> Text -> ProcessTrace
processRemarkTracedWith triageCfg abbrevCfg bpCfg entries rawRemarks =
    let (normalized, normEdits) = normalize abbrevCfg rawRemarks
        (cleaned, cleanEdits) = clean bpCfg normalized
        (scanned, hits) = scanAll entries (unCleanedText cleaned)
        triaged = triage triageCfg cleaned scanned
        fuzzy = FuzzySnapshot (confidenceScore triaged) (parserConfidence triaged)
     in ProcessTrace
            { ptInput = rawRemarks
            , ptNormalized = normalized
            , ptNormalizeEdits = normEdits
            , ptCleaned = cleaned
            , ptCleanEdits = cleanEdits
            , ptScanned = scanned
            , ptScanHits = hits
            , ptTriaged = triaged
            , ptFuzzy = fuzzy
            }
