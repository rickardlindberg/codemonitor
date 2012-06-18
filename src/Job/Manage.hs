module Job.Manage where

import Control.Concurrent
import Control.Exception
import Job.Types
import System.Exit
import System.IO
import System.Process
import Text.Regex.Posix

type Signaller = String -> Status -> String -> IO ()

runAllJobs :: Signaller -> Jobs -> IO Jobs
runAllJobs signalResult (Jobs jobs) = fmap Jobs (mapM (reRunJob signalResult) jobs)

reRunJobs :: FilePath -> Signaller -> Jobs -> IO Jobs
reRunJobs fileChanged signalResult (Jobs jobs) = fmap Jobs (mapM reRunIfMatch jobs)
    where
        reRunIfMatch job =
            if fileChanged =~ matchExpr job
                then reRunJob signalResult job
                else return job

reRunJob :: Signaller -> Job -> IO Job
reRunJob signalResult job = do
    cancel job
    -- NOTE: signalResult must be called asynchronoulsy, otherwise the lock for
    -- jobsRef will deadlock.
    threadId <- runThread job signalResult
    return $ job { runningInfo = RunningJobInfo Working "" (Just threadId), status = Working, output = "" }

cancel :: Job -> IO ()
cancel Job { runningInfo = RunningJobInfo _ _ (Just id) } = killThread id
cancel _                                                  = return ()

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


updateJobStatus :: String -> Status -> String -> Jobs -> Jobs
updateJobStatus theId status newOutput (Jobs jobs) = Jobs (map updateJobInner jobs)
    where
        updateJobInner job
            | jobId job == theId = job { status = status
                                       , output = output job ++ newOutput
                                       , runningInfo = RunningJobInfo status (output job ++ newOutput) Nothing
                                       }
            | otherwise          = job
