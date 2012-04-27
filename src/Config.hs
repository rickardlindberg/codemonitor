module Config where

import Job
import Monitor

create :: FilePath -> IO (String, Jobs, [Monitor])
create configPath = do
    content <- readFile configPath
    let (watchDir:rest) = lines content
    let jobs = map jobDefToJob rest
    let monitors = map jobToMonitor jobs
    return (watchDir, createJobs jobs, monitors)

jobDefToJob :: String -> Job
jobDefToJob def =
    let (id:pattern:command:args) = words def
    in processJob id command args pattern

jobToMonitor job = JobMonitor (jobId job) 0 (fullName job) (status job)
