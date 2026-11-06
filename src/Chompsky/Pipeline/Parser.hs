-- | Build scanner entries from Lua parser specs.
module Chompsky.Pipeline.Parser
    ( ScannerEntry (..)
    , Parser
    , allEntries
    ) where

import Chompsky.Config.ParserSpec (ParserSpec)
import Chompsky.Pipeline.Parser.Build (buildEntries)
import Chompsky.Pipeline.Parser.Internal (Parser, ScannerEntry (..))

{- | Order of entries does not affect which values are extracted, since each
entry scans the full text independently. It does set the order of
'Chompsky.Types.ScanHit's in a trace, which is why spec files load sorted.
-}
allEntries :: [ParserSpec] -> [ScannerEntry]
allEntries = concatMap buildEntries
