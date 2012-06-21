module Monitor where

import Job.Types

data Monitor = StatusCodeMonitor
                { mJobId          :: String
                , mJobName        :: String
                , mJobStatus      :: JobStatus
                , mOutput         :: String
                }
             | StdoutMonitor
                { mJobId          :: String
                , mJobName        :: String
                , mOutput         :: String
                }
             deriving (Show, Eq)

updateMonitors :: [RunningJob] -> [Monitor] -> [Monitor]
updateMonitors runningJobs = map updateMonitor
    where
        updateMonitor monitor@(StatusCodeMonitor {}) =
            case runningJobWithId (mJobId monitor) runningJobs of
                Nothing -> monitor
                Just x  -> monitor { mJobStatus = jobStatus x
                                   , mOutput    = jobOutput x
                                   }
        updateMonitor monitor@(StdoutMonitor {}) =
            case runningJobWithId (mJobId monitor)runningJobs of
                Nothing -> monitor
                Just x  -> monitor { mOutput = jobOutput x }
