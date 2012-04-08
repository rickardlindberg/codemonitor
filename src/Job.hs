module Job where

import Data.IORef
import System.Process

data Job = Job String [String] (Maybe ProcessHandle)

name :: Job -> String
name (Job name args _) = name

createJobs :: [Job]
createJobs = [ Job "ls" [] Nothing
             , Job "find" ["/home/rick", "README"] Nothing
             ]

updateJobs :: Maybe FilePath -> [Job] -> IO [Job]
updateJobs file = mapM updateJob
    where
        updateJob (Job name args Nothing) =
            case file of
                Just _ -> do
                    (_, _, _, handle) <- createProcess (proc name args)
                    return $ Job name args (Just handle)
                Nothing -> return $ Job name args Nothing
        updateJob (Job name args (Just h)) = do
            exitCode <- getProcessExitCode h
            case exitCode of
                Nothing -> return $ Job name args (Just h)
                _       -> return $ Job name args Nothing
        updateJob job = return job
