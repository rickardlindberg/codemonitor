module Job where

import Data.IORef
import System.Process

data Job = Job
    { name :: String
    , args :: [String]
    , process :: Maybe ProcessHandle
    }

createJobs :: [Job]
createJobs = [ Job "ls" [] Nothing
             , Job "find" ["/home/rick", "README"] Nothing
             ]

updateJobs :: Maybe FilePath -> [Job] -> IO [Job]
updateJobs file = mapM updateJob
    where
        updateJob job@(Job { process = Nothing }) =
            case file of
                Just _ -> do
                    (_, _, _, handle) <- createProcess (proc (name job) (args job))
                    return $ job { process = Just handle }
                Nothing -> return job
        updateJob job@(Job { process = Just h }) = do
            exitCode <- getProcessExitCode h
            case exitCode of
                Nothing -> return $ job { process = Just h }
                _       -> return $ job { process = Nothing }
