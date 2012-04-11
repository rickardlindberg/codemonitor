module Job where

import Control.Concurrent
import Data.List
import Prelude hiding (id)
import System.Exit
import System.Process
import Text.Regex.Posix

data Job = Job
    { name :: String
    , args :: [String]
    , matchExpr :: String
    , status :: Status
    , thread :: Maybe Thread
    }

data Thread = Thread
    { id :: ThreadId
    , result :: MVar Status
    }

data Status = Idle | Working | Fail String

fullName :: Job -> String
fullName (Job { name = name, args = args }) = name ++ " " ++ intercalate " " args

isFailed :: Job -> Bool
isFailed (Job { status = Fail _ }) = True
isFailed _ = False

processJob :: String -> [String] -> String -> Job
processJob name args expr = Job name args expr Idle Nothing

updateJobs :: Maybe FilePath -> [Job] -> IO [Job]
updateJobs file = mapM updateJob
    where
        updateJob job =
            if shouldStartThread job file
                then do
                    cancel job
                    mvar <- newEmptyMVar
                    threadId <- forkIO $ runThread job mvar
                    return $ job { thread = Just (Thread threadId mvar), status = Working }
                else do
                    value <- tryGetNewStatus job
                    case value of
                        Nothing -> return job
                        Just s  -> return $ job { thread = Nothing, status = s }

shouldStartThread :: Job -> Maybe FilePath -> Bool
shouldStartThread job (Just f) = f =~ matchExpr job
shouldStartThread _ _ = False

cancel :: Job -> IO ()
cancel Job { thread = Just (Thread { id = id }) } = killThread id
cancel _ = return ()

runThread :: Job -> MVar Status -> IO ()
runThread job result = do
    (exit, stdout, stderr) <- readProcessWithExitCode (name job) (args job) ""
    if exit == ExitSuccess
        then putMVar result Idle
        else putMVar result (Fail $ stderr ++ stdout)
    return ()

tryGetNewStatus Job { thread = Just (Thread { result = result }) } = tryTakeMVar result
tryGetNewStatus _ = return Nothing
