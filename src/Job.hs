module Job where

import Control.Concurrent
import Data.IORef
import System.Process

data Status = Idle | Working

data Job = Job
    { name :: String
    , args :: [String]
    , status :: Status
    , thread :: Maybe ThreadId
    , threadMvar :: MVar Status
    }

createJobs :: IO [Job]
createJobs = do
    m1 <- newEmptyMVar
    m2 <- newEmptyMVar
    m3 <- newEmptyMVar
    m4 <- newEmptyMVar
    return [ Job "ls" [] Idle Nothing m1
           , Job "sleep" ["1"] Idle Nothing m2
           , Job "sleep" ["2"] Idle Nothing m3
           , Job "sleep" ["3"] Idle Nothing m4
           ]

updateJobs :: Maybe FilePath -> [Job] -> IO [Job]
updateJobs file = mapM updateJob
    where
        updateJob job = do
            if shouldReRun job file
                then do
                    killIt job
                    threadId <- forkIO $ runThread job
                    return $ job { thread = Just threadId, status = Working }
                else do
                    value <- tryTakeMVar (threadMvar job)
                    case value of
                        Nothing -> return job
                        Just s -> return $ job { thread = Nothing, status = s }

killIt :: Job -> IO ()
killIt Job { thread = Just id } = killThread id
killIt _ = return ()

shouldReRun :: Job -> Maybe FilePath -> Bool
shouldReRun job (Just f) = f == "Job.hs"
shouldReRun _ _ = False

runThread :: Job -> IO ()
runThread job = do
    out <- readProcess (name job) (args job) ""
    putMVar (threadMvar job) Idle
    return ()
