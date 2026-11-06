{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Lua-driven parser spec types, Peeker functions, and loading.
module Chompsky.Config.ParserSpec
    ( ParserStrategy (..)
    , SpecEntry (..)
    , ParserSpec (..)
    , loadParserSpecs
    , validateSpec
    ) where

import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import HsLua.Core qualified as Lua
import HsLua.Marshalling
    ( Peeker
    , liftLua
    , peekFieldRaw
    , peekIntegral
    , peekList
    , peekText
    , retrieving
    )
import System.Directory (listDirectory)
import System.FilePath ((</>))

import Chompsky.Config.Sandbox (loadSandboxed, peekKnownKeys)

-- | How a parser spec entry assembles Megaparsec combinators.
data ParserStrategy
    = -- | Match literal phrases; tag label passed through as 'TagValue'.
      PhraseStrategy
    | -- | Prefix / adverb-direct / suffix forms with optional year.
      VerbalStrategy
    | -- | Trigger phrase followed by a 4-digit year.
      TriggerStrategy
    | -- | Dollar amount in prefix-dollar or label-dollar form.
      MonetaryStrategy
    | -- | Trigger phrase followed by greedy capture to a sentence boundary.
      CaptureStrategy
    deriving (Show, Eq)

-- | A single tag-to-patterns mapping within a parser spec.
data SpecEntry = SpecEntry
    { seTag :: Text
    -- ^ Opaque label, e.g. @"Solar"@, @"Roof"@. Becomes the tag text in output.
    , sePhrases :: [Text]
    -- ^ strategy=phrase: literal phrases to match.
    , seSynonyms :: [Text]
    -- ^ strategy=verbal: subject synonyms.
    , seSuffixExtra :: [Text]
    -- ^ strategy=verbal: extra synonyms for the suffix form only.
    , seTriggers :: [Text]
    -- ^ strategy=capture/trigger: trigger phrases.
    , seLabels :: [Text]
    -- ^ strategy=monetary: label prefixes.
    , seBridges :: [Text]
    -- ^ strategy=monetary: optional bridge words between label and @$@.
    , seFrequency :: [Text]
    -- ^ strategy=monetary: post-amount frequency indicators.
    , seTerminators :: Text
    -- ^ strategy=capture: characters that end the captured body.
    , seDirectPhrases :: [Text]
    -- ^ strategy=verbal: phrases matched after an adverb (adverb-direct form).
    }
    deriving (Show, Eq)

-- | Top-level parser spec loaded from a Lua file.
data ParserSpec = ParserSpec
    { psVersion :: Int
    , psCategory :: Text
    -- ^ @"category"@, @"activity"@, @"cost"@, etc.
    , psStrategy :: ParserStrategy
    , psEntries :: [SpecEntry]
    , psAdverbs :: [Text]
    -- ^ strategy=verbal: adverbs that optionally precede an action verb.
    , psActionVerbs :: [Text]
    -- ^ strategy=verbal: verbs that introduce a verbal prefix phrase.
    , psSuffixVerbs :: [Text]
    -- ^ strategy=verbal: verbs used in the suffix form (subject comes first).
    }
    deriving (Show, Eq)

peekListOrNil :: Peeker Lua.Exception Text -> Peeker Lua.Exception [Text]
peekListOrNil p idx = do
    ty <- liftLua (Lua.ltype idx)
    case ty of
        Lua.TypeNil -> pure []
        _ -> peekList p idx

peekTextOrNil :: Peeker Lua.Exception Text
peekTextOrNil idx = do
    ty <- liftLua (Lua.ltype idx)
    case ty of
        Lua.TypeNil -> pure ""
        _ -> peekText idx

-- | The only version of the parser spec format.
supportedVersion :: Int
supportedVersion = 1

peekSpecEntry :: Peeker Lua.Exception SpecEntry
peekSpecEntry idx =
    retrieving "SpecEntry" $
        peekKnownKeys
            [ "tag"
            , "phrases"
            , "synonyms"
            , "suffix_extra"
            , "triggers"
            , "labels"
            , "bridges"
            , "frequency"
            , "terminators"
            , "direct_phrases"
            ]
            idx
            *> ( SpecEntry
                    <$> peekFieldRaw peekText "tag" idx
                    <*> peekFieldRaw (peekListOrNil peekText) "phrases" idx
                    <*> peekFieldRaw (peekListOrNil peekText) "synonyms" idx
                    <*> peekFieldRaw (peekListOrNil peekText) "suffix_extra" idx
                    <*> peekFieldRaw (peekListOrNil peekText) "triggers" idx
                    <*> peekFieldRaw (peekListOrNil peekText) "labels" idx
                    <*> peekFieldRaw (peekListOrNil peekText) "bridges" idx
                    <*> peekFieldRaw (peekListOrNil peekText) "frequency" idx
                    <*> peekFieldRaw peekTextOrNil "terminators" idx
                    <*> peekFieldRaw (peekListOrNil peekText) "direct_phrases" idx
               )

peekParserSpec :: Peeker Lua.Exception ParserSpec
peekParserSpec idx = retrieving "ParserSpec" $ do
    peekKnownKeys
        ["version", "category", "strategy", "entries", "adverbs", "action_verbs", "suffix_verbs"]
        idx
    version <- peekFieldRaw peekIntegral "version" idx
    if version == supportedVersion
        then pure ()
        else fail ("unsupported spec version " <> show version <> "; expected " <> show supportedVersion)
    category <- peekFieldRaw peekText "category" idx
    stratText <- peekFieldRaw peekText "strategy" idx
    strategy <- case stratText of
        "phrase" -> pure PhraseStrategy
        "verbal" -> pure VerbalStrategy
        "trigger" -> pure TriggerStrategy
        "monetary" -> pure MonetaryStrategy
        "capture" -> pure CaptureStrategy
        other -> fail ("unknown parser strategy: " <> T.unpack other)
    entries <- peekFieldRaw (peekList peekSpecEntry) "entries" idx
    adverbs <- peekFieldRaw (peekListOrNil peekText) "adverbs" idx
    actionVerbs <- peekFieldRaw (peekListOrNil peekText) "action_verbs" idx
    suffixVerbs <- peekFieldRaw (peekListOrNil peekText) "suffix_verbs" idx
    pure (ParserSpec version category strategy entries adverbs actionVerbs suffixVerbs)

{- | Check that required fields are populated for the spec's strategy.
Returns 'Left' with a descriptive message on the first violation found.
-}
validateSpec :: ParserSpec -> Either String ()
validateSpec ps = checkSpecLevel >> mapM_ checkEntry (psEntries ps)
    where
        strat = psStrategy ps
        checkSpecLevel = do
            requireSpec "psCategory" (not (isBlank (psCategory ps)))
            requireSpec "psAdverbs" (noBlanks (psAdverbs ps))
            requireSpec "psActionVerbs" (noBlanks (psActionVerbs ps))
            requireSpec "psSuffixVerbs" (noBlanks (psSuffixVerbs ps))
            case strat of
                VerbalStrategy ->
                    requireSpec "psAdverbs" (not (null (psAdverbs ps)))
                        >> requireSpec "psActionVerbs" (not (null (psActionVerbs ps)))
                        >> requireSpec "psSuffixVerbs" (not (null (psSuffixVerbs ps)))
                _ -> Right ()
        requireSpec field ok
            | ok = Right ()
            | otherwise =
                Left $
                    T.unpack (psCategory ps)
                        <> " spec: "
                        <> field
                        <> " must be non-empty, with no blank entries"
        checkEntry e = checkEntryLiterals e >> checkEntryStrategy e
        -- A blank literal matches the empty string, so it would fire at every
        -- word start. Reject it in every list, whatever the strategy.
        checkEntryLiterals e = do
            require "seTag" (not (isBlank (seTag e))) e
            require "sePhrases" (noBlanks (sePhrases e)) e
            require "seSynonyms" (noBlanks (seSynonyms e)) e
            require "seSuffixExtra" (noBlanks (seSuffixExtra e)) e
            require "seTriggers" (noBlanks (seTriggers e)) e
            require "seLabels" (noBlanks (seLabels e)) e
            require "seBridges" (noBlanks (seBridges e)) e
            require "seFrequency" (noBlanks (seFrequency e)) e
            require "seDirectPhrases" (noBlanks (seDirectPhrases e)) e
        checkEntryStrategy e = case strat of
            PhraseStrategy -> require "sePhrases" (not (null (sePhrases e))) e
            VerbalStrategy -> require "seSynonyms" (not (null (seSynonyms e))) e
            TriggerStrategy -> require "seTriggers" (not (null (seTriggers e))) e
            MonetaryStrategy -> require "seLabels" (not (null (seLabels e))) e
            CaptureStrategy -> require "seTriggers" (not (null (seTriggers e))) e
        isBlank = T.null . T.strip
        noBlanks = not . any isBlank
        require field ok e
            | ok = Right ()
            | otherwise =
                Left $
                    "entry "
                        <> T.unpack (seTag e)
                        <> " in "
                        <> T.unpack (psCategory ps)
                        <> " spec: "
                        <> field
                        <> " must be non-empty, with no blank entries"

{- | Load all @.lua@ files from @configDir/parsers/@ in a single Lua VM, in
file-name order so the entry order (and so the hit order) is deterministic.
-}
loadParserSpecs :: FilePath -> IO [ParserSpec]
loadParserSpecs configDir = do
    let parsersDir = configDir </> "parsers"
    files <- listDirectory parsersDir
    let luaFiles = sort (filter (".lua" `isSuffixOf`) files)
    Lua.run $ mapM (loadOneSpecInVM . (parsersDir </>)) luaFiles

-- | Load and validate one spec file in the shared sandbox.
loadOneSpecInVM :: FilePath -> Lua.LuaE Lua.Exception ParserSpec
loadOneSpecInVM path = do
    spec <- loadSandboxed path peekParserSpec
    case validateSpec spec of
        Right () -> pure spec
        Left msg -> Lua.failLua (path <> ": " <> msg)
