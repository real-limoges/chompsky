{-# LANGUAGE OverloadedStrings #-}

-- | CSV input parsing for two-column (id, text) files.
module Chompsky.IO.CsvInput
    ( streamInputCsv
    , countInputRows
    , readInputCsv
    , readInputCsvLimit
    ) where

import Control.Exception (throwIO)
import Data.ByteString.Lazy qualified as BL
import Data.Csv.Streaming (Records (..))
import Data.Csv.Streaming qualified as CsvS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)

import Chompsky.Types (ChompskyError (..), InputRow (..))

{- | Feed each row of the file to @action@, in order, reading lazily so memory
stays flat however long the file is. Expects columns @id@ and @text@. A
leading UTF-8 byte-order mark (as written by Excel) is dropped, so it cannot
hide the first header name. Throws 'ChompskyError' on a bad header or row,
naming the row.
-}
streamInputCsv :: FilePath -> (InputRow -> IO ()) -> IO ()
streamInputCsv path action = do
    bs <- BL.readFile path
    let withoutBom = fromMaybe bs (BL.stripPrefix "\xEF\xBB\xBF" bs)
    case CsvS.decodeByName withoutBom of
        Left err -> parseError err
        Right (_, records) -> go (1 :: Int) records
    where
        go _ (Nil Nothing _) = pure ()
        go n (Nil (Just err) _) = parseError ("row " <> show n <> ": " <> err)
        go n (Cons (Left err) _) = parseError ("row " <> show n <> ": " <> err)
        go n (Cons (Right row) rest) = action row >> go (n + 1) rest
        parseError err = throwIO (ChompskyError (path <> ": CSV parse error: " <> err))

-- | Number of rows in the file, read the same way as 'streamInputCsv'.
countInputRows :: FilePath -> IO Int
countInputRows path = do
    counter <- newIORef 0
    streamInputCsv path (\_ -> modifyIORef' counter (+ 1))
    readIORef counter

-- | Every row of the file, in order.
readInputCsv :: FilePath -> IO [InputRow]
readInputCsv path = do
    rowsRef <- newIORef []
    streamInputCsv path (\row -> modifyIORef' rowsRef (row :))
    reverse <$> readIORef rowsRef

-- | Like 'readInputCsv' but returns at most @n@ rows.
readInputCsvLimit :: FilePath -> Int -> IO [InputRow]
readInputCsvLimit path n = take n <$> readInputCsv path
