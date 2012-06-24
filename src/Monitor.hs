module Monitor where

import Job.Scheduler

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

updateMonitors :: JobStatusUpdate -> [Monitor] -> [Monitor]
updateMonitors newStatus = map updateMonitor
    where
        updateMonitor monitor@(StatusCodeMonitor {}) =
            if sId newStatus == mJobId monitor
                then monitor { mJobStatus = sStatus newStatus
                             , mOutput    = sOutput newStatus
                             }
                else monitor
        updateMonitor monitor@(StdoutMonitor {}) =
            if sId newStatus == mJobId monitor
                then monitor { mOutput = sOutput newStatus }
                else monitor
