module Job where

import Control.Concurrent
import Job.Types
import System.Exit
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
    threadId <- forkIO $ runThread job signalResult
    return $ job { thread = Just threadId, status = Working, output = "" }

cancel :: Job -> IO ()
cancel Job { thread = Just id } = killThread id
cancel _                        = return ()

runThread :: Job -> Signaller -> IO ()
runThread job signalResult = do
    -- NOTE: Is the process killed if this thread is killed? If not, is that
    -- the reason why we get resource exhaustion sometimes?
    (exit, stdout, stderr) <- readProcessWithExitCode (name job) (args job) ""
    if exit == ExitSuccess
        then signalResult (jobId job) Idle (stderr ++ stdout)
        else signalResult (jobId job) Fail (stderr ++ stdout)

updateJobStatus :: String -> Status -> String -> Jobs -> Jobs
updateJobStatus theId status newOutput (Jobs jobs) = Jobs (map updateJobInner jobs)
    where
        updateJobInner job
            | jobId job == theId = job { status = status
                                       , thread = Nothing
                                       , output = output job ++ newOutput
                                       }
            | otherwise          = job
