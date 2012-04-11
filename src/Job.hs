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

createJobs :: [Job]
createJobs = [ Job "ls" [] "Main.hs" Idle Nothing
             , Job "sleep" ["1"] "\\.hs$" Idle Nothing
             , Job "sleep" ["2"] "\\.hs$" Idle Nothing
             , Job "hlint" ["src"] "\\.hs$" Idle Nothing
             , Job "sh" ["run-tests"] "\\.hs$" Idle Nothing
             ]

updateJobs :: Maybe FilePath -> [Job] -> IO [Job]
updateJobs file = mapM updateJob
    where
        updateJob job =
            if shouldReRun job file
                then do
                    killIt job
                    mvar <- newEmptyMVar
                    threadId <- forkIO $ runThread job mvar
                    return $ job { thread = Just (Thread threadId mvar), status = Working }
                else do
                    value <- myTry job
                    case value of
                        Nothing -> return job
                        Just s -> return $ job { thread = Nothing, status = s }

myTry Job { thread = Just (Thread _ mvar) } = tryTakeMVar mvar
myTry job = return Nothing

shouldReRun :: Job -> Maybe FilePath -> Bool
shouldReRun job (Just f) = f =~ matchExpr job
shouldReRun _ _ = False

killIt :: Job -> IO ()
killIt Job { thread = Just (Thread id _) } = killThread id
killIt _ = return ()

runThread :: Job -> MVar Status -> IO ()
runThread job mvar = do
    (exit, stdout, stderr) <- readProcessWithExitCode (name job) (args job) ""
    if exit == ExitSuccess
        then putMVar mvar Idle
        else putMVar mvar (Fail $ stderr ++ stdout)
    return ()
