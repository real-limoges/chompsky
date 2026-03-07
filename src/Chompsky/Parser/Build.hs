{-# LANGUAGE OverloadedStrings #-}

module Chompsky.Parser.Build
    ( buildEntries
    ) where


buildEntries :: ParserSpec -> [ScannerEntry]