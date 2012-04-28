module Main (main) where

import Config
import Control.Concurrent
import Data.IORef
import Data.Time.Clock
import Graphics.UI.Gtk
import Job
import Monitor
import Notifier
import Render
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

    args <- getArgs
    (watchDir, jobs, monitors) <- create (head args)

    lock <- newEmptyMVar
    jobsRef <- newIORef jobs
    let withJobLock = createWithJobLock lock jobsRef

    monitorsRef <- newIORef monitors

    t <- getCurrentTime
    timeRef <- newIORef t

    setupNotifications watchDir (onFileChanged withJobLock forceRedraw)
    onInit withJobLock forceRedraw

    timeoutAddFull (forceRedraw >> return True) priorityDefaultIdle 10

    mainWindow `onDestroy` mainQuit
    canvas     `onExpose`  redraw canvas timeRef jobsRef monitorsRef

    widgetShowAll mainWindow
    return ()

redraw canvas timeRef jobsRef monitorsRef event = do
    oldT <- readIORef timeRef
    t <- getCurrentTime
    writeIORef timeRef t
    let delta = realToFrac (diffUTCTime t oldT)

    (w, h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    jobs <- readIORef jobsRef
    modifyIORef monitorsRef (updateMonitors delta jobs)
    monitors <- readIORef monitorsRef
    renderWithDrawable drawin (renderScreen monitors (fromIntegral w) (fromIntegral h))
    return True

createWithJobLock :: MVar () -> IORef Jobs -> (Jobs -> IO Jobs) -> IO ()
createWithJobLock lock jobsRef fn = do
    putMVar lock ()
    jobs <- readIORef jobsRef
    newJobs <- fn jobs
    writeIORef jobsRef newJobs
    takeMVar lock

onInit :: ((Jobs -> IO Jobs) -> IO ()) -> IO () -> IO ()
onInit withJobLock forceRedraw = do
    withJobLock $ runAllJobs (onStatusChanged withJobLock forceRedraw)
    forceRedraw

onFileChanged :: ((Jobs -> IO Jobs) -> IO ()) -> IO () -> FilePath -> IO ()
onFileChanged withJobLock forceRedraw filePath = do
    withJobLock (reRunJobs filePath (onStatusChanged withJobLock forceRedraw))
    forceRedraw

onStatusChanged :: ((Jobs -> IO Jobs) -> IO ()) -> IO () -> String -> Status -> IO ()
onStatusChanged withJobLock forceRedraw id status = do
    withJobLock (return . updateJobStatus id status)
    forceRedraw
