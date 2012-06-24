module Main (main) where

import Config
import Control.Concurrent
import Data.IORef
import Graphics.UI.Gtk
import Job.Description
import Job.Running
import Monitor
import Notifier
import Render.Graphics
import System

main :: IO ()
main = do
    initGUI
    showMainWindow
    mainGUI

showMainWindow :: IO ()
showMainWindow = do
    mainWindow <- windowNew
    canvas <- drawingAreaNew
    set mainWindow [ windowTitle := "Code Monitor", containerChild := canvas ]

    let forceRedraw = postGUIAsync $ widgetQueueDraw canvas

    monitorsRef <- initOurStuff forceRedraw

    timeoutAddFull (forceRedraw >> return True) priorityDefaultIdle 100

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

initOurStuff forceRedraw = do
    (watchDir, jobDescriptions, monitors) <- readConfig

    monitorsRef <- newIORef monitors
    monitorsLock <- newEmptyMVar
    let statusUpdateHandler newStatus = do
        putMVar monitorsLock ()
        modifyIORef monitorsRef (updateMonitors newStatus)
        takeMVar monitorsLock
        forceRedraw

    runJobs <- newJobScheduler statusUpdateHandler

    setupNotifications watchDir (runJobs . filterJobsMatching jobDescriptions)

    runJobs jobDescriptions

    return monitorsRef

readConfig :: IO (String, [JobDescription], [Monitor])
readConfig = do
    args <- getArgs
    case args of
        [path] -> create path
        []     -> getContents >>= createFromConfig
