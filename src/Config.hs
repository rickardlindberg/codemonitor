module Config where

import Job.Types
import Monitor

create :: FilePath -> IO (String, [JobDescription], [Monitor])
create configPath = readFile configPath >>= createFromConfig

createFromConfig :: String -> IO (String, [JobDescription], [Monitor])
createFromConfig content = do
    let (watchDir:rest) = lines content
    let jobs = map jobDefToJob rest
    let monitors = map jobToMonitor jobs
    return (watchDir, jobs, monitors)

jobDefToJob :: String -> JobDescription
jobDefToJob def =
    let (id:pattern:command:args) = words def
    in JobDescription id command args pattern

jobToMonitor job =
    case jobId job of
        'o':'m':_ -> StdoutMonitor (jobId job) (fullName job) ""
        _         -> StatusCodeMonitor (jobId job) (fullName job) Idle ""
