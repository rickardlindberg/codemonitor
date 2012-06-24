module Job.Scheduler where

import Control.Concurrent
import Control.Exception
import Data.IORef
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

newJobScheduler :: (JobStatusUpdate -> IO ()) -> IO ([JobDescription] -> IO ())
newJobScheduler signaller = do
    lock <- newEmptyMVar
    threadsRef <- newIORef M.empty

    let killThreadForJobWithId id = do
        threads <- readIORef threadsRef
        case M.lookup id threads of
            Nothing       -> return ()
            Just threadId -> do killThread threadId
                                modifyIORef threadsRef (M.delete id)
    let runJob job = do
        threadId <- runThread job signaller
        modifyIORef threadsRef (M.insert (jobId job) threadId)

    let runJobs jobs = do
        putMVar lock ()
        mapM_ (killThreadForJobWithId . jobId) jobs
        mapM_ runJob jobs
        takeMVar lock
        return ()

    return runJobs

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
