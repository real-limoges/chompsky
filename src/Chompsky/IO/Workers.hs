{-# LANGUAGE StrictData #-}

-- | Streaming row processor with pre-built scanner entries and strict evaluation.
module Chompsky.IO.Workers
    ( processStreaming
    ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)

import Chompsky.Config (AppConfig (..), abbreviationConfig, boilerplateConfig, parserSpecs)
import Chompsky.Pipeline (ProcessResult, processRemarkPureWith)
import Chompsky.Pipeline.Parser (allEntries)
import Chompsky.Types (InputRow (..))

{- | Feed rows from @producer@ through the pipeline, handing each fully
evaluated result to @callback@ as soon as it is ready. Nothing is kept, so
memory stays flat; the callback decides what to do with each result.
Scanner entries are built once, up front.
-}
processStreaming ::
    AppConfig ->
    ((InputRow -> IO ()) -> IO ()) ->
    (InputRow -> ProcessResult -> IO ()) ->
    IO ()
processStreaming cfg producer callback = do
    let entries = allEntries (parserSpecs cfg)
        abbrev = abbreviationConfig cfg
        bp = boilerplateConfig cfg
        tCfg = triageConfig cfg
    producer $ \row -> do
        let result = processRemarkPureWith tCfg abbrev bp entries (rowText row)
        evaluated <- evaluate (force result)
        callback row evaluated
