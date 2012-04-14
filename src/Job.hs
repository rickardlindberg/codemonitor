module Job where

import Control.Concurrent
import System.Exit
import System.Process
import Text.Regex.Posix

data Job = Job
    { jobId :: String
    , name :: String
    , args :: [String]
    , matchExpr :: String
    , status :: Status
    , thread :: Maybe ThreadId
    }

data Status = Idle | Working | Fail String

fullName :: Job -> String
fullName (Job { name = name, args = args }) = name ++ " " ++ unwords args

isFailed :: Job -> Bool
isFailed (Job { status = Fail _ }) = True
isFailed _ = False

processJob :: String -> String -> [String] -> String -> Job
processJob jobId name args expr = Job jobId name args expr Idle Nothing

reRunJobs :: FilePath -> (String -> Status -> IO ()) -> [Job] -> IO [Job]
reRunJobs fileChanged signalResult = mapM reRunJob
    where
        reRunJob job =
            if shouldStartThread job fileChanged
                then do
                    cancel job
                    -- signalResult must be called asynchronoulsy, otherwise
                    -- the lock for jobsRef will deadlock.
                    threadId <- forkIO $ runThread job signalResult
                    return $ job { thread = Just threadId, status = Working }
                else
                    return job

shouldStartThread :: Job -> FilePath -> Bool
shouldStartThread job f = f =~ matchExpr job

cancel :: Job -> IO ()
cancel Job { thread = Just id } = killThread id
cancel _ = return ()

runThread :: Job -> (String -> Status -> IO ()) -> IO ()
runThread job signalResult = do
    (exit, stdout, stderr) <- readProcessWithExitCode (name job) (args job) ""
    if exit == ExitSuccess
        then signalResult (jobId job) Idle
        else signalResult (jobId job) (Fail $ stderr ++ stdout)

updateJobStatus :: String -> Status -> [Job] -> [Job]
updateJobStatus theId status jobs = map updateJobInner jobs
    where
        updateJobInner job
            | jobId job == theId = job { status = status, thread = Nothing }
            | otherwise          = job
