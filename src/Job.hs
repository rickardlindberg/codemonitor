module Job where

import Control.Concurrent
import Data.List
import System.Exit
import System.Process
import Text.Regex.Posix

data Job = Job
    { name :: String
    , args :: [String]
    , matchExpr :: String
    , status :: Status
    , thread :: Maybe ThreadId
    , threadMvar :: MVar Status
    }

data Status = Idle | Working | Fail String

fullName :: Job -> String
fullName (Job { name = name, args = args }) = name ++ " " ++ intercalate " " args

isFailed :: Job -> Bool
isFailed (Job { status = Fail _ }) = True
isFailed _ = False

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
           , Job "hlint" ["src"] "\\.hs$" Idle Nothing m4
           , Job "sh" ["run-tests"] "\\.hs$" Idle Nothing m5
           ]

updateJobs :: Maybe FilePath -> [Job] -> IO [Job]
updateJobs file = mapM updateJob
    where
        updateJob job =
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

shouldReRun :: Job -> Maybe FilePath -> Bool
shouldReRun job (Just f) = f =~ matchExpr job
shouldReRun _ _ = False

killIt :: Job -> IO ()
killIt Job { thread = Just id } = killThread id
killIt _ = return ()

runThread :: Job -> IO ()
runThread job = do
    (exit, stdout, stderr) <- readProcessWithExitCode (name job) (args job) ""
    if exit == ExitSuccess
        then putMVar (threadMvar job) Idle
        else putMVar (threadMvar job) (Fail $ stderr ++ stdout)
    return ()
