module Monitor where

import Job

data Monitor = JobMonitor { mJobId          :: String
                          , mSecondsInState :: Double
                          , mJobName        :: String
                          , mJobStatus      :: Status
                          }

updateMonitors :: Double -> Jobs -> [Monitor] -> [Monitor]
updateMonitors secondsSinceLastUpdate jobs = map updateMonitor
    where
        updateMonitor monitor@(JobMonitor {}) =
            let newStatus  = status $ jobWithId jobs (mJobId monitor)
                newSeconds = if mJobStatus monitor == newStatus
                                then secondsSinceLastUpdate + mSecondsInState monitor
                                else 0
            in monitor { mJobStatus      = newStatus
                       , mSecondsInState = newSeconds
                       }
