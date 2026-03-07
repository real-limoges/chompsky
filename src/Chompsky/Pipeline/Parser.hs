-- | Build scanner entries from Lua parser specs.
module Chompsky.Pipeline.Parser
    ( ScannerEntry (..)
    , Parser
    , allEntries
    ) where

import Chompsky.Config.ParserSpec (ParserSpec)
import Chompsky.Pipeline.Parser.Build (buildEntries)
import Chompsky.Pipeline.Parser.Internal (Parser, ScannerEntry (..))

-- | Order of entries does not affect correctness — each scans the full text independently.
allEntries :: [ParserSpec] -> [ScannerEntry]
allEntries = concatMap buildEntries
