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
    jobsRef    <- newIORef [Job "ls" [] Nothing,
                            Job "find" ["/home/rick", "README"] Nothing]
    builder    <- builderFromFile "interface.glade"
    mainWindow <- builderGetObject builder castToWindow "main_window"
    canvas     <- builderGetObject builder castToDrawingArea "canvas"
    mainWindow `onDestroy` mainQuit
    canvas     `onExpose`  redraw canvas jobsRef
    widgetShowAll mainWindow
    let x = do
        widgetQueueDraw canvas
        return True
    timeoutAdd x 10
    setupNotifications "src" (\a -> updateJobsRef (Just a) jobsRef)
    return ()

updateJobsRef :: Maybe FilePath -> IORef [Job] -> IO ()
updateJobsRef changedFile jobsRef = do
    jobs <- readIORef jobsRef
    newJobs <- updateJobs changedFile jobs
    writeIORef jobsRef newJobs

updateJobs :: Maybe FilePath -> [Job] -> IO [Job]
updateJobs file = mapM updateJob
    where
        updateJob (Job name args Nothing) =
            case file of
                Just _ -> do
                    (_, _, _, handle) <- createProcess (proc name args)
                    return $ Job name args (Just handle)
                Nothing -> return $ Job name args Nothing
        updateJob (Job name args (Just h)) = do
            exitCode <- getProcessExitCode h
            case exitCode of
                Nothing -> return $ Job name args (Just h)
                _       -> return $ Job name args Nothing
        updateJob job = return job

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
