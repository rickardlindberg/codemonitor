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
    return [ Job "ls" [] Idle Nothing m1
           , Job "find" ["/home/rick/downloads"] Idle Nothing m2
           ]

updateJobs :: Maybe FilePath -> [Job] -> IO [Job]
updateJobs file = mapM updateJob
    where
        updateJob job@(Job { thread = Nothing, threadMvar = mvar }) =
            case file of
                Just _ -> do
                    threadId <- forkIO $ runThread job mvar
                    return $ job { thread = Just threadId, status = Working }
                Nothing -> return job
        updateJob job@(Job { thread = Just _, threadMvar = mvar }) = do
            value <- tryTakeMVar mvar
            case value of
                Nothing -> return job
                Just s -> return $ job { thread = Nothing, status = s }

runThread :: Job -> MVar Status -> IO ()
runThread job mvar = do
    out <- readProcess (name job) (args job) ""
    putMVar mvar Idle
    return ()
