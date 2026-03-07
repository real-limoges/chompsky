{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

-- | Top-level pipeline composition: normalize, clean, scan, triage. Pure -- no IO.
module Chompsky.Pipeline
    ( processRemarkPure
    , processRemarkPureWith
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
import Chompsky.Types (CleanedText (..), Extraction, TriageConfig)

data ProcessResult = ProcessResult
    { prExtraction :: Extraction
    , prCleanedText :: CleanedText
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
    let normalized = normalize abbrevCfg rawRemarks
        cleaned = clean bpCfg normalized
        extraction = scanAll entries (unCleanedText cleaned)
        triaged = triage triageCfg cleaned extraction
     in ProcessResult triaged cleaned
