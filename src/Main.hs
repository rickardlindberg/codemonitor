module Main (main) where

import Config
import Monitor.ErrorFile
import Control.Concurrent
import Data.IORef
import GUI.MainWindow
import Job.Description
import Job.Scheduler
import Monitor
import Notifier
import System

main :: IO ()
main = do
    showMainWindow setupMonitors
    removeErrorFile

setupMonitors :: IO () -> IO (IORef [Monitor])
setupMonitors forceRedraw = do
    (watchDir, jobDescriptions, monitors) <- readConfig

    monitorsRef <- newIORef monitors
    monitorsLock <- newEmptyMVar
    let statusUpdateHandler newStatus = do
        putMVar monitorsLock ()
        modifyIORef monitorsRef (updateMonitors newStatus)
        readIORef monitorsRef >>= writeErrorFile
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
