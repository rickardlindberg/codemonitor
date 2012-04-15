module Main (main) where

import Control.Concurrent
import Data.IORef
import Graphics.UI.Gtk
import Job
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

    lock       <- newEmptyMVar
    jobsRef    <- newIORef createJobsFromDefinitions

    let forceRedraw = postGUIAsync $ widgetQueueDraw canvas
    setupNotifications "src" (\a -> updateJobsRef (Just a) jobsRef forceRedraw lock)
    updateJobsRef Nothing jobsRef forceRedraw lock

    timeoutAddFull (yield >> return True) priorityDefaultIdle 100

    mainWindow `onDestroy` mainQuit
    canvas     `onExpose`  redraw canvas jobsRef lock

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

redraw canvas jobsRef lock event = do
    (w, h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    jobs <- readIORef jobsRef
    renderWithDrawable drawin (renderScreen jobs (fromIntegral w) (fromIntegral h))
    return True

updateJobsRef :: Maybe FilePath -> IORef Jobs -> IO () -> MVar () -> IO ()
updateJobsRef changedFile jobsRef forceRedraw lock = do
    putMVar lock ()
    jobs <- readIORef jobsRef
    newJobs <- case changedFile of
                 Just f  -> reRunJobs f updateJobRef jobs
                 Nothing -> runAllJobs updateJobRef jobs
    writeIORef jobsRef newJobs
    takeMVar lock
    forceRedraw
    where
        updateJobRef id status = do
            putMVar lock ()
            jobs <- readIORef jobsRef
            writeIORef jobsRef (updateJobStatus id status jobs)
            takeMVar lock
            forceRedraw
