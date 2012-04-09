module Job where

import Control.Concurrent
import Data.IORef
import System.Process
import Text.Regex.Posix

data Status = Idle | Working

data Job = Job
    { name :: String
    , args :: [String]
    , matchExpr :: String
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
    m5 <- newEmptyMVar
    return [ Job "ls" [] "Main.hs" Idle Nothing m1
           , Job "sleep" ["1"] "\\.hs$" Idle Nothing m2
           , Job "sleep" ["2"] "\\.hs$" Idle Nothing m3
           , Job "sleep" ["3"] "\\.hs$" Idle Nothing m4
           , Job "sh" ["run-tests"] "\\.hs$" Idle Nothing m5
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
shouldReRun job (Just f) = f =~ (matchExpr job)
shouldReRun _ _ = False

runThread :: Job -> IO ()
runThread job = do
    out <- readProcess (name job) (args job) ""
    putMVar (threadMvar job) Idle
    return ()
