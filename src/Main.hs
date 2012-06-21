module Main (main) where

import Config
import Control.Concurrent
import Data.IORef
import Graphics.UI.Gtk
import Job.Description
import Job.Manage
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

    runningJobsRef <- newIORef []
    monitorsRef    <- newIORef monitors

    lock <- newEmptyMVar
    let mutateGlobalState fn = do
        -- aquire lock
        putMVar lock ()
        -- modify refs
        runningJobs <- readIORef runningJobsRef
        newRunning  <- fn jobDescriptions runningJobs
        modifyIORef monitorsRef (updateMonitors newRunning)
        writeIORef runningJobsRef newRunning
        -- release lock
        takeMVar lock
        -- redraw since state has changed
        forceRedraw

    let jobFinishedHandler id status newOutput = mutateGlobalState (myFn3 storeJobResult id status newOutput)

    setupNotifications watchDir $ \filePath ->
        mutateGlobalState (reRunJobs filePath jobFinishedHandler)

    mutateGlobalState (runAllJobs jobFinishedHandler)

    return monitorsRef

myFn3 storeJobResult id status newOutput jobDescriptions runningJobs = return (storeJobResult id status newOutput jobDescriptions runningJobs)

readConfig :: IO (String, [JobDescription], [Monitor])
readConfig = do
    args <- getArgs
    case args of
        [path] -> create path
        []     -> getContents >>= createFromConfig
