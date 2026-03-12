{-# LANGUAGE StrictData #-}

-- | Streaming row processor with pre-built scanner entries and strict evaluation.
module Chompsky.IO.Workers
    ( processStreaming
    ) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.IORef

import Chompsky.Config (AppConfig (..), abbreviationConfig, boilerplateConfig, parserSpecs)
import Chompsky.Pipeline (ProcessResult, processRemarkPureWith)
import Chompsky.Pipeline.Parser (allEntries)
import Chompsky.Types (InputRow (..))

-- | Feed rows through the pipeline via @producer@, invoking @callback@ after each.
processStreaming ::
    AppConfig ->
    ((InputRow -> IO ()) -> IO ()) ->
    (InputRow -> ProcessResult -> IO ()) ->
    IO [(InputRow, ProcessResult)]
processStreaming cfg producer callback = do
    let entries = allEntries (parserSpecs cfg)
        abbrev = abbreviationConfig cfg
        bp = boilerplateConfig cfg
        tCfg = triageConfig cfg
    resultsRef <- newIORef []
    producer $ \row -> do
        let result = processRemarkPureWith tCfg abbrev bp entries (rowText row)
        evaluated <- evaluate (force result)
        callback row evaluated
        modifyIORef' resultsRef ((row, evaluated) :)
    reverse <$> readIORef resultsRef
