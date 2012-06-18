module Monitor where

import Job.Types

data Monitor = StatusCodeMonitor
                { mJobId          :: String
                , mSecondsInState :: Double
                , mJobName        :: String
                , mJobStatus      :: Status
                , mOutput         :: String
                }
             | StdoutMonitor
                { mJobId          :: String
                , mSecondsInState :: Double
                , mJobName        :: String
                , mOutput         :: String
                }
             deriving (Show, Eq)

updateMonitors :: Double -> RunningJobInfos -> [Monitor] -> [Monitor]
updateMonitors secondsSinceLastUpdate runningInfos = map updateMonitor
    where
        updateMonitor monitor@(StatusCodeMonitor {}) =
            let newStatus  = jobStatus $ runningInfoWithId runningInfos (mJobId monitor)
                newOutput  = jobOutput $ runningInfoWithId runningInfos (mJobId monitor)
                newSeconds = if mJobStatus monitor == newStatus
                                then secondsSinceLastUpdate + mSecondsInState monitor
                                else 0
            in monitor { mSecondsInState = newSeconds
                       , mJobStatus      = newStatus
                       , mOutput         = newOutput
                       }
        updateMonitor monitor@(StdoutMonitor {}) =
            let newOutput  = jobOutput $ runningInfoWithId runningInfos (mJobId monitor)
                newSeconds = secondsSinceLastUpdate + mSecondsInState monitor
            in monitor { mSecondsInState = newSeconds
                       , mOutput         = newOutput
                       }

