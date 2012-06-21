module Job.Manage where

import Control.Concurrent
import Control.Exception
import Job.Types
import System.Exit
import System.IO
import System.Process
import Text.Regex.Posix

type Signaller = String -> Status -> String -> IO ()

runAllJobs :: Signaller -> Jobs -> RunningJobInfos -> IO RunningJobInfos
runAllJobs signalResult (Jobs jobs) runningInfos =
    fmap RunningJobInfos (mapM (reRunJob signalResult runningInfos) jobs)

reRunJobs :: FilePath -> Signaller -> Jobs -> RunningJobInfos -> IO RunningJobInfos
reRunJobs fileChanged signalResult (Jobs jobs) runningInfos = do
    let matchingJobs = filter isMatch jobs
    newInfos <- mapM (reRunJob signalResult runningInfos) matchingJobs
    return $ mergeTwoInfos (RunningJobInfos newInfos) runningInfos
    where
        isMatch job = fileChanged =~ matchExpr job

reRunJob :: Signaller -> RunningJobInfos -> Job -> IO RunningJobInfo
reRunJob signalResult runningInfos job = do
    cancel $ runningJobInfoWithId runningInfos (jobId job)
    -- NOTE: signalResult must be called asynchronously, otherwise the lock for
    -- jobsRef will deadlock.
    threadId <- runThread job signalResult
    let info = RunningJobInfo (jobId job) Working "" (Just threadId)
    return info

cancel :: Maybe RunningJobInfo -> IO ()
cancel (Just (RunningJobInfo _ _ _ (Just id))) = killThread id
cancel _                                       = return ()

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

storeJobResult :: String -> Status -> String -> Jobs -> RunningJobInfos -> RunningJobInfos
storeJobResult jobId newStatus newOutput jobs runningInfos =
    processRunningInfos updateInfo runningInfos
    where
        updateInfo info
            | runningJobId info == jobId = Just $ RunningJobInfo jobId newStatus (jobOutput info ++ newOutput) Nothing
            | otherwise                  = Just info
