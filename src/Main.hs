module Main (main) where

import Control.Concurrent
import Data.IORef
import Data.Time.Clock
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

    monitorsRef <- newIORef [ JobMonitor "job1" 0 "" Idle
                            , JobMonitor "job2" 0 "" Idle
                            , JobMonitor "job3" 0 "" Idle
                            , JobMonitor "job4" 0 "" Idle
                            , JobMonitor "job5" 0 "" Idle
                            ]


    t <- getCurrentTime
    timeRef <- newIORef t

    setupNotifications "src" (onFileChanged withJobLock forceRedraw)
    onInit withJobLock forceRedraw

    timeoutAddFull (forceRedraw >> return True) priorityDefaultIdle 10

    mainWindow `onDestroy` mainQuit
    canvas     `onExpose`  redraw canvas timeRef jobsRef monitorsRef

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

updateMonitors :: Double -> Jobs -> [Monitor] -> [Monitor]
updateMonitors d jobs = map updateMonitor
    where
        updateMonitor monitor@(JobMonitor { mJobId = id, mJobStatus = currentStatus, mTimeInState = dd }) =
            let newName    = fullName $ jobWithId jobs id
                newStatus  = status   $ jobWithId jobs id
                newMonitor = monitor { mJobName   = newName
                                     , mJobStatus = newStatus
                                     }
            in if currentStatus == newStatus
                then newMonitor { mTimeInState = d + dd }
                else newMonitor { mTimeInState = 0 }

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
