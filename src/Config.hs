module Config where

import Job.Types
import Monitor

create :: FilePath -> IO (String, Jobs, [Monitor])
create configPath = readFile configPath >>= createFromConfig

createFromConfig :: String -> IO (String, Jobs, [Monitor])
createFromConfig content = do
    let (watchDir:rest) = lines content
    let jobs = map jobDefToJob rest
    let monitors = map jobToMonitor jobs
    return (watchDir, createJobs jobs, monitors)

jobDefToJob :: String -> Job
jobDefToJob def =
    let (id:pattern:command:args) = words def
    in createJob id command args pattern

jobToMonitor job =
    case jobId job of
        'o':'m':_ -> StdoutMonitor (jobId job) (fullName job) ""
        _         -> StatusCodeMonitor (jobId job) (fullName job) Idle ""
