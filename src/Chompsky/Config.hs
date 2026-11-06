{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Load abbreviation, boilerplate, and triage config from Lua files.
module Chompsky.Config
    ( -- * Opaque config types (accessor-only)
      AbbreviationConfig
    , wordReplacements
    , phraseReplacements
    , BoilerplateConfig
    , boilerplatePhrases
    , minPosition

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
import Chompsky.Config.Sandbox (loadSandboxed, peekKnownKeys)
import Chompsky.Types (TriageConfig (..))

-- | Abbreviation expansion rules loaded from @abbreviations.lua@.
data AbbreviationConfig = AbbreviationConfig
    { wordReplacements :: HashMap Text Text
    -- ^ Single-word substitutions applied with word-boundary guards.
    , phraseReplacements :: [(Text, Text)]
    -- ^ Multi-word phrase replacements, sorted longest-first.
    }
    deriving (Show, Eq)

-- | Boilerplate-stripping rules loaded from @boilerplate.lua@.
data BoilerplateConfig = BoilerplateConfig
    { boilerplatePhrases :: [Text]
    -- ^ Phrases to remove from input text.
    , minPosition :: Int
    -- ^ Character position threshold; phrases before this offset are kept.
    }
    deriving (Show, Eq)

-- | Aggregate configuration for the entire pipeline, loaded from a config directory.
data AppConfig = AppConfig
    { abbreviationConfig :: AbbreviationConfig
    , boilerplateConfig :: BoilerplateConfig
    , parserSpecs :: [ParserSpec]
    -- ^ Loaded from @parsers/*.lua@.
    , triageConfig :: TriageConfig
    -- ^ Triage-stage thresholds and watch lists.
    }
    deriving (Show, Eq)

-- | Load all config from @configDir@ (abbreviations, boilerplate, triage, parsers/).
loadConfig :: FilePath -> IO AppConfig
loadConfig configDir = do
    abbrevCfg <- loadLuaFile (configDir </> "abbreviations.lua") peekAbbreviationConfig abbreviationKeys
    bpCfg <- loadLuaFile (configDir </> "boilerplate.lua") peekBoilerplateConfig boilerplatePhrases
    triageCfg <- loadLuaFile (configDir </> "triage.lua") peekTriageConfig triagePhrases
    specs <- loadParserSpecs configDir
    pure
        AppConfig
            { abbreviationConfig = abbrevCfg
            , boilerplateConfig = bpCfg
            , parserSpecs = specs
            , triageConfig = triageCfg
            }

{- | Load one config file through the shared sandbox, then reject it if it
holds a blank phrase. A blank phrase is not just useless: @T.breakOn ""@
calls 'error', so it would crash the pipeline on the first row.
-}
loadLuaFile :: FilePath -> Peeker Lua.Exception a -> (a -> [Text]) -> IO a
loadLuaFile path peeker phrasesOf = Lua.run $ do
    cfg <- loadSandboxed path peeker
    if any (T.null . T.strip) (phrasesOf cfg)
        then Lua.failLua (path <> ": phrases and words must not be blank")
        else pure cfg

{- Every phrase and word below is lowercased at load. The pipeline lowercases
its input before Normalize, and the abbreviation lookup, boilerplate search
and triage matching all compare exactly, so an uppercase entry would never
match.
-}

peekAbbreviationConfig :: Peeker Lua.Exception AbbreviationConfig
peekAbbreviationConfig idx =
    retrieving "AbbreviationConfig" $
        peekKnownKeys ["words", "phrases"] idx
            *> ( buildAbbrev
                    <$> peekFieldRaw (peekKeyValuePairs peekText peekText) "words" idx
                    <*> peekFieldRaw (peekList peekPhraseEntry) "phrases" idx
               )

buildAbbrev :: [(Text, Text)] -> [(Text, Text)] -> AbbreviationConfig
buildAbbrev wordPairs phraseList =
    AbbreviationConfig
        { wordReplacements = HashMap.fromList [(T.toLower k, v) | (k, v) <- wordPairs]
        , phraseReplacements =
            sortBy (comparing (Down . T.length . fst)) [(T.toLower k, v) | (k, v) <- phraseList]
        }

abbreviationKeys :: AbbreviationConfig -> [Text]
abbreviationKeys cfg = HashMap.keys (wordReplacements cfg) ++ map fst (phraseReplacements cfg)

peekPhraseEntry :: Peeker Lua.Exception (Text, Text)
peekPhraseEntry idx =
    retrieving "PhraseEntry" $
        peekKnownKeys ["match", "replace"] idx
            *> ( (,)
                    <$> peekFieldRaw peekText "match" idx
                    <*> peekFieldRaw peekText "replace" idx
               )

peekBoilerplateConfig :: Peeker Lua.Exception BoilerplateConfig
peekBoilerplateConfig idx =
    retrieving "BoilerplateConfig" $
        peekKnownKeys ["phrases", "min_position"] idx
            *> ( BoilerplateConfig
                    <$> peekFieldRaw (peekList peekLowerText) "phrases" idx
                    <*> peekFieldRaw peekIntegral "min_position" idx
               )

triagePhrases :: TriageConfig -> [Text]
triagePhrases cfg =
    concatMap snd (tcAmbiguousPhrases cfg)
        ++ concatMap snd (tcVocabWatchLists cfg)

peekLowerText :: Peeker Lua.Exception Text
peekLowerText idx = T.toLower <$> peekText idx

peekAmbiguousEntry :: Peeker Lua.Exception (Text, [Text])
peekAmbiguousEntry idx =
    retrieving "AmbiguousEntry" $
        peekKnownKeys ["category", "phrases"] idx
            *> ( (,)
                    <$> peekFieldRaw peekText "category" idx
                    <*> peekFieldRaw (peekList peekLowerText) "phrases" idx
               )

peekVocabEntry :: Peeker Lua.Exception (Text, [Text])
peekVocabEntry idx =
    retrieving "VocabEntry" $
        peekKnownKeys ["category", "words"] idx
            *> ( (,)
                    <$> peekFieldRaw peekText "category" idx
                    <*> peekFieldRaw (peekList peekLowerText) "words" idx
               )

peekConflictEntry :: Peeker Lua.Exception (Text, [Text], [Text])
peekConflictEntry idx =
    retrieving "ConflictEntry" $
        peekKnownKeys ["category", "positive", "negative"] idx
            *> ( (,,)
                    <$> peekFieldRaw peekText "category" idx
                    <*> peekFieldRaw (peekList peekText) "positive" idx
                    <*> peekFieldRaw (peekList peekText) "negative" idx
               )

peekTriageConfig :: Peeker Lua.Exception TriageConfig
peekTriageConfig idx =
    retrieving "TriageConfig" $
        peekKnownKeys
            [ "ambiguous_phrases"
            , "vocab_watch_lists"
            , "conflicting_tag_pairs"
            , "long_no_signal_threshold"
            , "vocab_threshold"
            , "unrecognizable_ratio_threshold"
            ]
            idx
            *> ( TriageConfig
                    <$> peekFieldRaw (peekList peekAmbiguousEntry) "ambiguous_phrases" idx
                    <*> peekFieldRaw (peekList peekVocabEntry) "vocab_watch_lists" idx
                    <*> peekFieldRaw (peekList peekConflictEntry) "conflicting_tag_pairs" idx
                    <*> peekFieldRaw peekIntegral "long_no_signal_threshold" idx
                    <*> peekFieldRaw peekIntegral "vocab_threshold" idx
                    <*> peekFieldRaw peekRealFloat "unrecognizable_ratio_threshold" idx
               )
