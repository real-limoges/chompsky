{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Load abbreviation, boilerplate, and triage config from Lua files.
module Chompsky.Config
    ( -- * Opaque config types (accessor-only)
      AbbreviationConfig
    , wordReplacements
    , phraseReplacements
    , buildAbbrev
    , BoilerplateConfig
    , boilerplatePhrases
    , minPosition
    , buildBoilerplate

      -- * Transparent aggregate config
    , AppConfig (..)
    , loadConfig
    ) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Text qualified as T
import HsLua.Core qualified as Lua
import HsLua.Marshalling
    ( Peeker
    , forcePeek
    , peekFieldRaw
    , peekIntegral
    , peekKeyValuePairs
    , peekList
    , peekRealFloat
    , peekText
    , retrieving
    )
import System.FilePath ((</>))

import Chompsky.Config.ParserSpec (ParserSpec, loadParserSpecs)
import Chompsky.Types (TriageConfig (..))

data AbbreviationConfig = AbbreviationConfig
    { wordReplacements :: HashMap Text Text
    , phraseReplacements :: [(Text, Text)] -- sorted longest-first
    }
    deriving (Show, Eq)

data BoilerplateConfig = BoilerplateConfig
    { boilerplatePhrases :: [Text]
    , minPosition :: Int
    }
    deriving (Show, Eq)

data AppConfig = AppConfig
    { abbreviationConfig :: AbbreviationConfig
    , boilerplateConfig :: BoilerplateConfig
    , parserSpecs :: [ParserSpec]
    , triageConfig :: TriageConfig
    }
    deriving (Show, Eq)

-- | Load all config from @configDir@ (abbreviations, boilerplate, triage, parsers/).
loadConfig :: FilePath -> IO AppConfig
loadConfig configDir = do
    abbrevCfg <- loadLuaFile (configDir </> "abbreviations.lua") peekAbbreviationConfig
    bpCfg <- loadLuaFile (configDir </> "boilerplate.lua") peekBoilerplateConfig
    triageCfg <- loadLuaFile (configDir </> "triage.lua") peekTriageConfig
    specs <- loadParserSpecs configDir
    pure
        AppConfig
            { abbreviationConfig = abbrevCfg
            , boilerplateConfig = bpCfg
            , parserSpecs = specs
            , triageConfig = triageCfg
            }

loadLuaFile :: FilePath -> Peeker Lua.Exception a -> IO a
loadLuaFile path peeker = Lua.run $ do
    status <- Lua.dofile (Just path)
    case status of
        Lua.OK -> forcePeek $ peeker Lua.top
        _ -> do
            msg <- forcePeek $ peekText Lua.top
            Lua.failLua (T.unpack msg)

peekAbbreviationConfig :: Peeker Lua.Exception AbbreviationConfig
peekAbbreviationConfig idx =
    retrieving "AbbreviationConfig" $
        buildAbbrev
            <$> peekFieldRaw (peekKeyValuePairs peekText peekText) "words" idx
            <*> peekFieldRaw (peekList peekPhraseEntry) "phrases" idx

buildAbbrev :: [(Text, Text)] -> [(Text, Text)] -> AbbreviationConfig
buildAbbrev wordPairs phraseList =
    AbbreviationConfig
        { wordReplacements = HashMap.fromList wordPairs
        , phraseReplacements = sortBy (comparing (Down . T.length . fst)) phraseList
        }

buildBoilerplate :: [Text] -> Int -> BoilerplateConfig
buildBoilerplate phrases pos =
    BoilerplateConfig
        { boilerplatePhrases = phrases
        , minPosition = pos
        }

peekPhraseEntry :: Peeker Lua.Exception (Text, Text)
peekPhraseEntry idx =
    retrieving "PhraseEntry" $
        (,)
            <$> peekFieldRaw peekText "match" idx
            <*> peekFieldRaw peekText "replace" idx

peekBoilerplateConfig :: Peeker Lua.Exception BoilerplateConfig
peekBoilerplateConfig idx =
    retrieving "BoilerplateConfig" $
        BoilerplateConfig
            <$> peekFieldRaw (peekList peekText) "phrases" idx
            <*> peekFieldRaw peekIntegral "min_position" idx

peekAmbiguousEntry :: Peeker Lua.Exception (Text, [Text])
peekAmbiguousEntry idx =
    retrieving "AmbiguousEntry" $
        (,)
            <$> peekFieldRaw peekText "category" idx
            <*> peekFieldRaw (peekList peekText) "phrases" idx

peekVocabEntry :: Peeker Lua.Exception (Text, [Text])
peekVocabEntry idx =
    retrieving "VocabEntry" $
        (,)
            <$> peekFieldRaw peekText "category" idx
            <*> peekFieldRaw (peekList peekText) "words" idx

peekConflictEntry :: Peeker Lua.Exception (Text, [Text], [Text])
peekConflictEntry idx =
    retrieving "ConflictEntry" $
        (,,)
            <$> peekFieldRaw peekText "category" idx
            <*> peekFieldRaw (peekList peekText) "positive" idx
            <*> peekFieldRaw (peekList peekText) "negative" idx

peekTriageConfig :: Peeker Lua.Exception TriageConfig
peekTriageConfig idx =
    retrieving "TriageConfig" $
        TriageConfig
            <$> peekFieldRaw (peekList peekAmbiguousEntry) "ambiguous_phrases" idx
            <*> peekFieldRaw (peekList peekVocabEntry) "vocab_watch_lists" idx
            <*> peekFieldRaw (peekList peekConflictEntry) "conflicting_tag_pairs" idx
            <*> peekFieldRaw peekIntegral "long_no_signal_threshold" idx
            <*> peekFieldRaw peekIntegral "vocab_threshold" idx
            <*> peekFieldRaw peekRealFloat "unrecognizable_ratio_threshold" idx
