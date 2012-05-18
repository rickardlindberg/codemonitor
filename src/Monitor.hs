module Monitor where

import Job

data Monitor = StatusCodeMonitor
                { mJobId          :: String
                , mSecondsInState :: Double
                , mJobName        :: String
                , mJobStatus      :: Status
                }
             | StdoutMonitor
                { mJobId          :: String
                , mSecondsInState :: Double
                , mJobName        :: String
                , mOutput         :: String
                }

updateMonitors :: Double -> Jobs -> [Monitor] -> [Monitor]
updateMonitors secondsSinceLastUpdate jobs = map updateMonitor
    where
        updateMonitor monitor@(StatusCodeMonitor {}) =
            let newStatus  = status $ jobWithId jobs (mJobId monitor)
                newSeconds = if mJobStatus monitor == newStatus
                                then secondsSinceLastUpdate + mSecondsInState monitor
                                else 0
            in monitor { mJobStatus      = newStatus
                       , mSecondsInState = newSeconds
                       }
        updateMonitor monitor = monitor
