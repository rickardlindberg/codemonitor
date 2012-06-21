module Job.Running where

import Control.Concurrent
import Control.Exception
import Job.Description
import qualified Data.Map as M
import System.Exit
import System.IO
import System.Process

data JobStatus
    = Idle
    | Working
    | Fail
    deriving (Eq, Show)

data JobStatusUpdate = JobStatusUpdate
    { sId     :: String
    , sStatus :: JobStatus
    , sOutput :: String
    }

data RunningJobs = RunningJobs (M.Map String ThreadId)

emptyRunningJobs :: RunningJobs
emptyRunningJobs = RunningJobs M.empty

removeRunning :: String -> RunningJobs -> RunningJobs
removeRunning id (RunningJobs map) = RunningJobs $ M.delete id map

threadIdForJobWithId :: String -> RunningJobs -> Maybe ThreadId
threadIdForJobWithId id (RunningJobs map) = M.lookup id map

runAllJobs :: (JobStatusUpdate -> IO ()) -> [JobDescription] -> RunningJobs -> IO RunningJobs
runAllJobs signalResult jobDescriptions runningJobs = do
    x <- mapM (reRunJob signalResult runningJobs) jobDescriptions
    return $ RunningJobs (M.fromList x)

reRunJobs :: FilePath -> (JobStatusUpdate -> IO ()) -> [JobDescription] -> RunningJobs -> IO RunningJobs
reRunJobs fileChanged signalResult jobDescriptions runningJobs = do
    let matchingJobs = filterJobsMatching fileChanged jobDescriptions
    newInfos <- mapM (reRunJob signalResult runningJobs) matchingJobs
    let (RunningJobs x) = runningJobs
    return $ RunningJobs $ M.union (M.fromList newInfos) x

reRunJob :: (JobStatusUpdate -> IO ()) -> RunningJobs -> JobDescription -> IO (String, ThreadId)
reRunJob signalResult runningJobs job = do
    cancel $ threadIdForJobWithId (jobId job) runningJobs
    -- NOTE: signalResult must be called asynchronously, otherwise the lock for
    -- jobsRef will deadlock.
    threadId <- runThread job signalResult
    return (jobId job, threadId)

cancel :: Maybe ThreadId -> IO ()
cancel (Just id) = killThread id
cancel _         = return ()

runThread :: JobDescription -> (JobStatusUpdate -> IO ()) -> IO ThreadId
runThread job signalResult = do
    (_, Just hOut, Just hErr, pid) <-
        createProcess (proc (name job) (args job))
            { std_out = CreatePipe , std_err = CreatePipe }

    let waitForProcessToFinish = do
        signalResult $ JobStatusUpdate (jobId job) Working ""

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
            then signalResult $ JobStatusUpdate (jobId job) Idle (err ++ out)
            else signalResult $ JobStatusUpdate (jobId job) Fail (err ++ out)

    forkIO $ onException waitForProcessToFinish (terminateProcess pid)
