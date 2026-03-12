{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Brick-based terminal UI for the backfill command.
module Chompsky.TUI
    ( runTUI
    , withLineBuffering
    ) where

import Brick (App (..), BrickEvent (..), EventM, Widget, customMain, halt, modify, neverShowCursor)
import Brick.AttrMap (attrMap)
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Core (Padding (..), hBox, padLeft, str, vBox)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, withAsync)
import Control.Concurrent.STM (TChan, atomically, readTChan)
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform qualified as VtyCP
import System.IO (BufferMode (..), hGetBuffering, hSetBuffering, stderr, stdout)

import Chompsky.TUI.State
    ( Phase (..)
    , TUIState (..)
    , initialState
    , updateState
    )
import Chompsky.TUI.Widgets (formatElapsed, formatEta, formatPct, formatRate, progressBar)
import Chompsky.Types (BackfillEvent (..))

type Name = ()

data TUIEvent
    = TUIBackfillEvent BackfillEvent
    | Tick

app :: App TUIState TUIEvent Name
app =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = pure ()
        , appAttrMap = const (attrMap V.defAttr [])
        }

drawUI :: TUIState -> [Widget Name]
drawUI st = [ui]
    where
        phase = tsPhase st
        processed = tsProcessed st
        total = tsTotal st
        flagged = tsFlagged st
        elapsed = tsElapsedSecs st

        phaseLabel = case phase of
            Fetching -> "Fetching"
            Processing -> "Processing"
            Writing -> "Writing"
            Done -> "Done"

        barWidth = 38

        confStr =
            "H:"
                <> show (tsHighConf st)
                <> "  M:"
                <> show (tsMedConf st)
                <> "  L:"
                <> show (tsLowConf st)

        ui =
            borderWithLabel (str " chompsky ") $
                vBox
                    [ hBox [str "Phase    ", str phaseLabel]
                    , hBorder
                    , progressBar barWidth processed total
                    , hBox
                        [ str (show processed <> "/" <> show total)
                        , padLeft Max (str (formatElapsed elapsed))
                        ]
                    , hBorder
                    , hBox [str "Rate     ", str (formatRate processed elapsed)]
                    , hBox [str "ETA      ", str (formatEta total processed elapsed)]
                    , hBorder
                    , hBox [str "Flagged  ", str (show flagged <> " (" <> formatPct flagged processed <> ")")]
                    , hBox [str "Conf.    ", str confStr]
                    , hBorder
                    , str "q  quit    ctrl+c  quit"
                    ]

handleEvent :: BrickEvent Name TUIEvent -> EventM Name TUIState ()
handleEvent ev = case ev of
    AppEvent (TUIBackfillEvent bev) -> do
        now <- liftIO getCurrentTime
        modify (updateState now bev)
        finished <- gets tsFinished
        when finished halt
    AppEvent Tick -> do
        now <- liftIO getCurrentTime
        modify $ \s -> case tsStartTime s of
            Nothing -> s
            Just t ->
                let elapsed = realToFrac (diffUTCTime now t) :: Double
                 in s {tsElapsedSecs = elapsed}
    VtyEvent (V.EvKey V.KEsc _) -> halt
    VtyEvent (V.EvKey (V.KChar 'q') _) -> halt
    VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
    _ -> pure ()

-- | Bridge events from the worker 'TChan' into Brick; returns on finish or quit.
runTUI :: TChan BackfillEvent -> IO ()
runTUI chan = do
    bchan <- newBChan 100
    withAsync (bridgeThread bchan) $ \_ -> do
        ticker <- async (tickThread bchan)
        let buildVty = VtyCP.mkVty V.defaultConfig
        initialVty <- buildVty
        finalSt <- customMain initialVty buildVty (Just bchan) app initialState
        cancel ticker
        printSummary finalSt
    where
        bridgeThread bc = do
            ev <- atomically (readTChan chan)
            writeBChan bc (TUIBackfillEvent ev)
            case ev of
                BackfillFinished -> pure ()
                _ -> bridgeThread bc
        tickThread bc = do
            threadDelay 100_000 -- 100ms
            writeBChan bc Tick
            tickThread bc

-- | Exported for testing.
formatSummary :: TUIState -> String
formatSummary st =
    "Done. Processed "
        <> show (tsProcessed st)
        <> " rows ("
        <> show (tsFlagged st)
        <> " flagged) in "
        <> formatElapsed (tsElapsedSecs st)
        <> " — "
        <> formatRate (tsProcessed st) (tsElapsedSecs st)
        <> " | Conf H:"
        <> show (tsHighConf st)
        <> " M:"
        <> show (tsMedConf st)
        <> " L:"
        <> show (tsLowConf st)

printSummary :: TUIState -> IO ()
printSummary = putStrLn . formatSummary

-- | Run an action with stdout/stderr set to line-buffering, restoring on exit.
withLineBuffering :: IO a -> IO a
withLineBuffering action =
    bracket acquire restore (const action)
    where
        acquire = do
            outMode <- hGetBuffering stdout
            errMode <- hGetBuffering stderr
            hSetBuffering stdout LineBuffering
            hSetBuffering stderr LineBuffering
            pure (outMode, errMode)
        restore (outMode, errMode) = do
            hSetBuffering stdout outMode
            hSetBuffering stderr errMode
