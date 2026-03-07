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

import Data.List (isSuffixOf)
import Data.Text (Text)
import Data.Text qualified as T
import HsLua.Core qualified as Lua
import HsLua.Marshalling
    ( Peeker
    , forcePeek
    , liftLua
    , peekFieldRaw
    , peekIntegral
    , peekList
    , peekText
    , retrieving
    )
import System.Directory (listDirectory)
import System.FilePath ((</>))

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
    -- ^ @"amenity"@, @"condition"@, @"renovation"@, etc.
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

peekOptionalList :: Peeker Lua.Exception Text -> Peeker Lua.Exception [Text]
peekOptionalList p idx = do
    ty <- liftLua (Lua.ltype idx)
    case ty of
        Lua.TypeNil -> pure []
        _ -> peekList p idx

peekOptionalText :: Peeker Lua.Exception Text
peekOptionalText idx = do
    ty <- liftLua (Lua.ltype idx)
    case ty of
        Lua.TypeNil -> pure ""
        _ -> peekText idx

peekSpecEntry :: Peeker Lua.Exception SpecEntry
peekSpecEntry idx =
    retrieving "SpecEntry" $
        SpecEntry
            <$> peekFieldRaw peekText "tag" idx
            <*> peekFieldRaw (peekOptionalList peekText) "phrases" idx
            <*> peekFieldRaw (peekOptionalList peekText) "synonyms" idx
            <*> peekFieldRaw (peekOptionalList peekText) "suffix_extra" idx
            <*> peekFieldRaw (peekOptionalList peekText) "triggers" idx
            <*> peekFieldRaw (peekOptionalList peekText) "labels" idx
            <*> peekFieldRaw (peekOptionalList peekText) "bridges" idx
            <*> peekFieldRaw (peekOptionalList peekText) "frequency" idx
            <*> peekFieldRaw peekOptionalText "terminators" idx
            <*> peekFieldRaw (peekOptionalList peekText) "direct_phrases" idx

peekParserSpec :: Peeker Lua.Exception ParserSpec
peekParserSpec idx = retrieving "ParserSpec" $ do
    version <- peekFieldRaw peekIntegral "version" idx
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
    adverbs <- peekFieldRaw (peekOptionalList peekText) "adverbs" idx
    actionVerbs <- peekFieldRaw (peekOptionalList peekText) "action_verbs" idx
    suffixVerbs <- peekFieldRaw (peekOptionalList peekText) "suffix_verbs" idx
    pure (ParserSpec version category strategy entries adverbs actionVerbs suffixVerbs)

{- | Check that required fields are populated for the spec's strategy.
Returns 'Left' with a descriptive message on the first violation found.
-}
validateSpec :: ParserSpec -> Either String ()
validateSpec ps = checkSpecLevel >> mapM_ checkEntry (psEntries ps)
    where
        strat = psStrategy ps
        checkSpecLevel = case strat of
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
                        <> " must be non-empty"
        checkEntry e = case strat of
            PhraseStrategy ->
                require "sePhrases" (not (null (sePhrases e))) e
            VerbalStrategy ->
                require "seSynonyms" (not (null (seSynonyms e))) e
                    >> require "seTag" (not (T.null (seTag e))) e
            TriggerStrategy -> require "seTriggers" (not (null (seTriggers e))) e
            MonetaryStrategy -> require "seLabels" (not (null (seLabels e))) e
            CaptureStrategy -> require "seTriggers" (not (null (seTriggers e))) e
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
                        <> " must be non-empty"

-- | Load all @.lua@ files from @configDir/parsers/@ in a single Lua VM.
loadParserSpecs :: FilePath -> IO [ParserSpec]
loadParserSpecs configDir = do
    let parsersDir = configDir </> "parsers"
    files <- listDirectory parsersDir
    let luaFiles = filter (".lua" `isSuffixOf`) files
    Lua.run $ mapM (loadOneSpecInVM . (parsersDir </>)) luaFiles

-- | Load one spec file; pops the result so subsequent files start clean.
loadOneSpecInVM :: FilePath -> Lua.LuaE Lua.Exception ParserSpec
loadOneSpecInVM path = do
    loadStatus <- Lua.loadfile (Just path)
    case loadStatus of
        Lua.OK -> do
            -- Restrict the chunk to an empty environment: no globals, no stdlib,
            -- no side-effects. Config files are pure data tables.
            Lua.newtable
            _ <- Lua.setupvalue (Lua.StackIndex (-2)) 1
            callStatus <- Lua.pcall 0 1 Nothing
            case callStatus of
                Lua.OK -> do
                    spec <- forcePeek $ peekParserSpec Lua.top
                    Lua.pop 1
                    case validateSpec spec of
                        Right () -> pure spec
                        Left msg -> Lua.failLua msg
                _ -> do
                    msg <- forcePeek $ peekText Lua.top
                    Lua.failLua (T.unpack msg)
        _ -> do
            msg <- forcePeek $ peekText Lua.top
            Lua.failLua (T.unpack msg)
