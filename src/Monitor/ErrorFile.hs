module Monitor.ErrorFile where

import Control.Exception
import Control.Monad
import Job.Scheduler
import Monitor
import System.Directory
import System.IO.Error

errorFileName = "codemonitor.errors"

writeErrorFile :: [Monitor] -> IO ()
writeErrorFile monitors = writeFile errorFileName (errorOutput monitors)

removeErrorFile :: IO ()
removeErrorFile = do
    tryJust (guard . isDoesNotExistError) (removeFile errorFileName)
    return ()

errorOutput :: [Monitor] -> String
errorOutput [] = ""
errorOutput (x:xs) = e x ++ "\n" ++ errorOutput xs
    where
        e (StatusCodeMonitor _ _ Fail output) = output
        e _                                   = ""
