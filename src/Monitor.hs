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

updateMonitors :: Double -> Jobs -> [Monitor] -> [Monitor]
updateMonitors secondsSinceLastUpdate jobs = map updateMonitor
    where
        updateMonitor monitor@(StatusCodeMonitor {}) =
            let newStatus  = status $ jobWithId jobs (mJobId monitor)
                newOutput  = output $ jobWithId jobs (mJobId monitor)
                newSeconds = if mJobStatus monitor == newStatus
                                then secondsSinceLastUpdate + mSecondsInState monitor
                                else 0
            in monitor { mSecondsInState = newSeconds
                       , mJobStatus      = newStatus
                       , mOutput         = newOutput
                       }
        updateMonitor monitor@(StdoutMonitor {}) =
            let newOutput  = output $ jobWithId jobs (mJobId monitor)
                newSeconds = secondsSinceLastUpdate + mSecondsInState monitor
            in monitor { mSecondsInState = newSeconds
                       , mOutput         = newOutput
                       }
