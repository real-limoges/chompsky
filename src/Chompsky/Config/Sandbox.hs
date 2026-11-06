{-# LANGUAGE OverloadedStrings #-}

{- | Load a Lua config file as pure data, in an empty environment, and check
the shape of what it returns.

Every config file (abbreviations, boilerplate, triage and each parser spec)
goes through 'loadSandboxed', so all of them share one sandbox policy.
See docs/reference/internal/design-decisions.md ("Sandboxed Lua config loading").
-}
module Chompsky.Config.Sandbox
    ( loadSandboxed
    , peekKnownKeys
    , instructionLimit
    ) where

import Data.Text (Text)
import Data.Text qualified as T
import HsLua.Core qualified as Lua
import HsLua.Marshalling (Peeker, forcePeek, peekKeyValuePairs, peekText)

{- | Compile @path@, replace the chunk's @_ENV@ upvalue with a fresh empty
table, run it under 'instructionLimit', and peek the returned value. The
chunk sees no globals and no standard library, and any global it assigns
lands in the throwaway table. Pops the result so the next file starts on a
clean stack.
-}
loadSandboxed :: FilePath -> Peeker Lua.Exception a -> Lua.LuaE Lua.Exception a
loadSandboxed path peeker = do
    loadStatus <- Lua.loadfile (Just path)
    case loadStatus of
        Lua.OK -> do
            -- Stack: [chunk]. Push the empty table, then pop it into the
            -- chunk's first upvalue, which Lua reserves for _ENV.
            Lua.newtable
            _ <- Lua.setupvalue (Lua.StackIndex (-2)) 1
            limitInstructions
            callStatus <- Lua.pcall 0 1 Nothing
            case callStatus of
                Lua.OK -> do
                    value <- forcePeek (peeker Lua.top)
                    Lua.pop 1
                    pure value
                _ -> failWithTopMessage
        _ -> failWithTopMessage
    where
        failWithTopMessage = do
            msg <- forcePeek (peekText Lua.top)
            Lua.failLua (path <> ": " <> T.unpack msg)

{- | Lua VM instructions a config file may run before loading is aborted.
Real config files return a table literal in a few hundred instructions; the
limit exists so a file that loops fails instead of hanging the program.
-}
instructionLimit :: Lua.Integer
instructionLimit = 10_000_000

{- | Install a count hook that raises an error after 'instructionLimit'
instructions. The debug library is opened only to reach @debug.sethook@ and
is never put where the chunk could see it. Leaves the stack as it found it.
-}
limitInstructions :: Lua.LuaE Lua.Exception ()
limitInstructions = do
    Lua.opendebug
    _ <- Lua.getfield Lua.top "sethook"
    Lua.pushHaskellFunction (Lua.failLua "config exceeded the instruction limit (does it loop?)")
    Lua.pushstring ""
    Lua.pushinteger instructionLimit
    Lua.call 3 0
    Lua.pop 1

{- | Fail if the table at @idx@ has a string key outside @allowed@, naming the
unknown keys. Catches a misspelled optional field, which would otherwise be
silently ignored.
-}
peekKnownKeys :: [Text] -> Peeker Lua.Exception ()
peekKnownKeys allowed idx = do
    keys <- map fst <$> peekKeyValuePairs peekText (\_ -> pure ()) idx
    case filter (`notElem` allowed) keys of
        [] -> pure ()
        unknown -> fail ("unknown keys: " <> T.unpack (T.intercalate ", " unknown))
