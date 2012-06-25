module Monitor.ErrorFile where

import Job.Scheduler
import Monitor
import System.Directory

errorFileName = "codemonitor.errors"

writeErrorFile :: [Monitor] -> IO ()
writeErrorFile monitors = writeFile errorFileName (errorOutput monitors)

removeErrorFile :: IO ()
removeErrorFile = removeFile errorFileName

errorOutput :: [Monitor] -> String
errorOutput [] = ""
errorOutput (x:xs) = e x ++ "\n" ++ errorOutput xs
    where
        e (StatusCodeMonitor _ _ Fail output) = output
        e _                                   = ""
