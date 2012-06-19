module Monitor where

import Job.Types

data Monitor = StatusCodeMonitor
                { mJobId          :: String
                , mJobName        :: String
                , mJobStatus      :: Status
                , mOutput         :: String
                }
             | StdoutMonitor
                { mJobId          :: String
                , mJobName        :: String
                , mOutput         :: String
                }
             deriving (Show, Eq)

updateMonitors :: RunningJobInfos -> [Monitor] -> [Monitor]
updateMonitors runningInfos = map updateMonitor
    where
        updateMonitor monitor@(StatusCodeMonitor {}) =
            case runningJobInfoWithId runningInfos (mJobId monitor) of
                Nothing -> monitor
                Just x  -> monitor { mJobStatus = jobStatus x
                                   , mOutput    = jobOutput x
                                   }
        updateMonitor monitor@(StdoutMonitor {}) =
            case runningJobInfoWithId runningInfos (mJobId monitor) of
                Nothing -> monitor
                Just x  -> monitor { mOutput = jobOutput x }
