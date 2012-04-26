module Main (main) where

import Control.Concurrent
import Data.IORef
import Graphics.UI.Gtk
import Job
import Monitor
import Notifier
import Render

main :: IO ()
main = do
    initGUI
    showMainWindow
    mainGUI

showMainWindow :: IO ()
showMainWindow = do
    builder    <- builderFromFile "interface.glade"
    mainWindow <- builderGetObject builder castToWindow "main_window"
    canvas     <- builderGetObject builder castToDrawingArea "canvas"
    let forceRedraw = postGUIAsync $ widgetQueueDraw canvas

    lock <- newEmptyMVar
    jobsRef <- newIORef createJobsFromDefinitions
    let withJobLock = createWithJobLock lock jobsRef

    setupNotifications "src" (onFileChanged withJobLock forceRedraw)
    onInit withJobLock forceRedraw

    timeoutAddFull (yield >> return True) priorityDefaultIdle 100

    mainWindow `onDestroy` mainQuit
    canvas     `onExpose`  redraw canvas jobsRef

    widgetShowAll mainWindow
    return ()

builderFromFile :: FilePath -> IO Builder
builderFromFile path = do
    builder <- builderNew
    builderAddFromFile builder path
    return builder

createJobsFromDefinitions :: Jobs
createJobsFromDefinitions = createJobs
    [ processJob "job1" "ls" [] "Main.hs"
    , processJob "job2" "sleep" ["1"] "\\.hs$"
    , processJob "job3" "sleep" ["2"] "\\.hs$"
    , processJob "job4" "hlint" ["src"] "\\.hs$"
    , processJob "job5" "sh" ["run-tests"] "\\.hs$"
    ]

redraw canvas jobsRef event = do
    (w, h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    jobs <- readIORef jobsRef
    let monitors = [ JobMonitor (fullName (jobWithId jobs "job1")) (status (jobWithId jobs "job1"))
                   , JobMonitor (fullName (jobWithId jobs "job2")) (status (jobWithId jobs "job2"))
                   , JobMonitor (fullName (jobWithId jobs "job3")) (status (jobWithId jobs "job3"))
                   , JobMonitor (fullName (jobWithId jobs "job4")) (status (jobWithId jobs "job4"))
                   , JobMonitor (fullName (jobWithId jobs "job5")) (status (jobWithId jobs "job5"))
                   ]
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
