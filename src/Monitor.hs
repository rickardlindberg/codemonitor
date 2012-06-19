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
            let newStatus  = jobStatus $ runningInfoWithId runningInfos (mJobId monitor)
                newOutput  = jobOutput $ runningInfoWithId runningInfos (mJobId monitor)
            in monitor { mJobStatus      = newStatus
                       , mOutput         = newOutput
                       }
        updateMonitor monitor@(StdoutMonitor {}) =
            let newOutput  = jobOutput $ runningInfoWithId runningInfos (mJobId monitor)
            in monitor { mOutput         = newOutput
                       }

