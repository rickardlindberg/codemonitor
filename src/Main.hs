module Main (main) where

import Data.IORef
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Job
import Notifier
import Render
import System.Process

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

    jobsRef    <- newIORef createJobs
    timeoutAdd (widgetQueueDraw canvas >> return True) 10
    setupNotifications "src" (\a -> updateJobsRef (Just a) jobsRef)

    mainWindow `onDestroy` mainQuit
    canvas     `onExpose`  redraw canvas jobsRef

    widgetShowAll mainWindow
    return ()

builderFromFile :: FilePath -> IO Builder
builderFromFile path = do
    builder <- builderNew
    builderAddFromFile builder path
    return builder

redraw canvas jobsRef event = do
    updateJobsRef Nothing jobsRef
    (w, h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    jobs <- readIORef jobsRef
    renderWithDrawable drawin (renderScreen jobs (fromIntegral w) (fromIntegral h))
    return True

updateJobsRef :: Maybe FilePath -> IORef [Job] -> IO ()
updateJobsRef changedFile jobsRef = do
    jobs <- readIORef jobsRef
    newJobs <- updateJobs changedFile jobs
    writeIORef jobsRef newJobs
