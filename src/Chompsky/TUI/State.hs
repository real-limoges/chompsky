{-# LANGUAGE StrictData #-}

-- | TUI state type and pure state-transition function for backfill progress.
module Chompsky.TUI.State
    ( TUIState (..)
    , Phase (..)
    , initialState
    , updateState
    ) where

import Data.Time.Clock (UTCTime)

import Chompsky.Types (BackfillEvent (..), Confidence (..))

data Phase = Fetching | Processing | Writing | Done
    deriving (Show, Eq)

data TUIState = TUIState
    { tsTotal :: Int
    , tsProcessed :: Int
    , tsFlagged :: Int
    , tsHighConf :: Int
    , tsMedConf :: Int
    , tsLowConf :: Int
    , tsStartTime :: Maybe UTCTime
    , tsElapsedSecs :: Double
    , tsPhase :: Phase
    , tsFinished :: Bool
    }

initialState :: TUIState
initialState =
    TUIState
        { tsTotal = 0
        , tsProcessed = 0
        , tsFlagged = 0
        , tsHighConf = 0
        , tsMedConf = 0
        , tsLowConf = 0
        , tsStartTime = Nothing
        , tsElapsedSecs = 0
        , tsPhase = Fetching
        , tsFinished = False
        }

-- | Pure state transition. `now` is only used for InputFetchDone to record start time.
updateState :: UTCTime -> BackfillEvent -> TUIState -> TUIState
updateState now ev st = case ev of
    InputFetchStarted -> st {tsPhase = Fetching}
    InputFetchDone n ->
        st
            { tsTotal = n
            , tsPhase = Processing
            , tsStartTime = Just now
            }
    ItemProcessed flagged conf ->
        st
            { tsProcessed = tsProcessed st + 1
            , tsFlagged = tsFlagged st + if flagged then 1 else 0
            , tsHighConf = tsHighConf st + case conf of High -> 1; _ -> 0
            , tsMedConf = tsMedConf st + case conf of Medium -> 1; _ -> 0
            , tsLowConf = tsLowConf st + case conf of Low -> 1; _ -> 0
            }
    CsvWriteStarted -> st {tsPhase = Writing}
    CsvWriteDone -> st
    BackfillFinished -> st {tsPhase = Done, tsFinished = True}
