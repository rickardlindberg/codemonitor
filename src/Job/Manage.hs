module Job.Manage where

import Control.Concurrent
import Control.Exception
import Job.Types
import System.Exit
import System.IO
import System.Process
import Text.Regex.Posix

type Signaller = String -> Status -> String -> IO ()

runAllJobs :: Signaller -> Jobs -> RunningJobInfos -> IO (Jobs, RunningJobInfos)
runAllJobs signalResult (Jobs jobs) runningInfos = do
    x <- fmap Jobs (mapM (reRunJob signalResult) jobs)
    let y = jobsToRunningJobInfos x
    return (x, y)

reRunJobs :: FilePath -> Signaller -> Jobs -> RunningJobInfos -> IO (Jobs, RunningJobInfos)
reRunJobs fileChanged signalResult (Jobs jobs) runningInfos = do
    x <- fmap Jobs (mapM reRunIfMatch jobs)
    let y = jobsToRunningJobInfos x
    return (x, y)
    where
        reRunIfMatch job =
            if fileChanged =~ matchExpr job
                then reRunJob signalResult job
                else return job

reRunJob :: Signaller -> Job -> IO Job
reRunJob signalResult job = do
    cancel $ runningInfo job
    -- NOTE: signalResult must be called asynchronously, otherwise the lock for
    -- jobsRef will deadlock.
    threadId <- runThread job signalResult
    return $ job { runningInfo = RunningJobInfo (jobId job) Working "" (Just threadId) }

cancel :: RunningJobInfo -> IO ()
cancel (RunningJobInfo _ _ _ (Just id) ) = killThread id
cancel _                                 = return ()

runThread :: Job -> Signaller -> IO ThreadId
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


updateJobStatus :: String -> Status -> String -> Jobs -> RunningJobInfos -> (Jobs, RunningJobInfos)
updateJobStatus theId status newOutput (Jobs jobs) runningInfos =
    let x = Jobs (map updateJobInner jobs)
        y = jobsToRunningJobInfos x
    in (x, y)
    where
        updateJobInner job
            | jobId job == theId = job { runningInfo = RunningJobInfo theId status (jobOutput (runningInfo job) ++ newOutput) Nothing
                                       }
            | otherwise          = job
