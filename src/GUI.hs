module GUI where

import Control.Concurrent
import Data.IORef
import Graphics.UI.Gtk
import Render.Graphics

showMainWindow setupMonitors = do
    initGUI
    setupMainWindow setupMonitors
    mainGUI

setupMainWindow setupMonitors = do
    mainWindow <- windowNew
    canvas <- drawingAreaNew
    set mainWindow [ windowTitle := "Code Monitor", containerChild := canvas ]

    let forceRedraw = postGUIAsync $ widgetQueueDraw canvas

    monitorsRef <- setupMonitors forceRedraw

    timeoutAddFull (yield >> return True) priorityDefaultIdle 100

    mainWindow `onDestroy` mainQuit
    canvas     `onExpose`  redraw canvas monitorsRef

    widgetShowAll mainWindow
    return ()

redraw canvas monitorsRef event = do
    (w, h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    monitors <- readIORef monitorsRef
    renderWithDrawable drawin (renderScreen monitors (fromIntegral w) (fromIntegral h))
    return True
