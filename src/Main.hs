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

    jobsRef    <- newIORef createJobs
    timeoutAdd (widgetQueueDraw canvas >> return True) 10
    setupNotifications "src" (\a -> updateJobsRef (Just a) jobsRef lock)

    mainWindow `onDestroy` mainQuit
    canvas     `onExpose`  redraw canvas jobsRef lock

    widgetShowAll mainWindow
    return ()

builderFromFile :: FilePath -> IO Builder
builderFromFile path = do
    builder <- builderNew
    builderAddFromFile builder path
    return builder

createJobs :: [Job]
createJobs = [ processJob "ls" [] "Main.hs"
             , processJob "sleep" ["1"] "\\.hs$"
             , processJob "sleep" ["2"] "\\.hs$"
             , processJob "hlint" ["src"] "\\.hs$"
             , processJob "sh" ["run-tests"] "\\.hs$"
             ]

redraw canvas jobsRef lock event = do
    updateJobsRef Nothing jobsRef lock
    (w, h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    jobs <- readIORef jobsRef
    renderWithDrawable drawin (renderScreen jobs (fromIntegral w) (fromIntegral h))
    return True

updateJobsRef :: Maybe FilePath -> IORef [Job] -> MVar () -> IO ()
updateJobsRef changedFile jobsRef lock = do
    putMVar lock ()
    jobs <- readIORef jobsRef
    newJobs <- updateJobs changedFile jobs
    writeIORef jobsRef newJobs
    takeMVar lock
