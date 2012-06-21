module Job.Manage where

import Control.Concurrent
import Control.Exception
import Data.Maybe
import Job.Running
import Job.Types
import System.Exit
import System.IO
import System.Process

type Signaller = String -> JobStatus -> String -> IO ()

runAllJobs :: Signaller -> [JobDescription] -> [RunningJob] -> IO [RunningJob]
runAllJobs signalResult jobDescriptions runningJobs =
    mapM (reRunJob signalResult runningJobs) jobDescriptions

reRunJobs :: FilePath -> Signaller -> [JobDescription] -> [RunningJob] -> IO [RunningJob]
reRunJobs fileChanged signalResult jobDescriptions runningJobs = do
    let matchingJobs = filterJobsMatching fileChanged jobDescriptions
    newInfos <- mapM (reRunJob signalResult runningJobs) matchingJobs
    return $ mergeTwoInfos newInfos runningJobs

reRunJob :: Signaller -> [RunningJob] -> JobDescription -> IO RunningJob
reRunJob signalResult runningJobs job = do
    cancel $ runningJobWithId (jobId job) runningJobs
    -- NOTE: signalResult must be called asynchronously, otherwise the lock for
    -- jobsRef will deadlock.
    threadId <- runThread job signalResult
    let info = RunningJob (jobId job) Working "" (Just threadId)
    return info

cancel :: Maybe RunningJob -> IO ()
cancel (Just (RunningJob _ _ _ (Just id))) = killThread id
cancel _                                   = return ()

runThread :: JobDescription -> Signaller -> IO ThreadId
runThread job signalResult = do
    (_, Just hOut, Just hErr, pid) <-
        createProcess (proc (name job) (args job))
            { std_out = CreatePipe , std_err = CreatePipe }

    let waitForProcessToFinish = do

        outMVar <- newEmptyMVar

        -- fork off a thread to start consuming stdout
        out <- hGetContents hOut
        _ <- forkIO $ evaluate (length out) >> putMVar outMVar ()

        -- fork off a thread to start consuming stderr
        err <- hGetContents hErr
        _ <- forkIO $ evaluate (length err) >> putMVar outMVar ()

        -- wait on the output
        takeMVar outMVar
        takeMVar outMVar
        hClose hOut
        hClose hErr

        -- wait on the process
        exit <- waitForProcess pid

        if exit == ExitSuccess
            then signalResult (jobId job) Idle (err ++ out)
            else signalResult (jobId job) Fail (err ++ out)

    forkIO $ onException waitForProcessToFinish (terminateProcess pid)

storeJobResult :: String -> JobStatus -> String -> [JobDescription] -> [RunningJob] -> [RunningJob]
storeJobResult jobId newStatus newOutput jobDescriptions runningInfos =
    mapMaybe updateInfo runningInfos
    where
        updateInfo info
            | runningJobId info == jobId = Just $ RunningJob jobId newStatus (jobOutput info ++ newOutput) Nothing
            | otherwise                  = Just info
