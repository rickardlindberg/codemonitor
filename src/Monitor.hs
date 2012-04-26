module Monitor where

import Job

data Monitor = JobMonitor { mJobId       :: String
                          , mTimeInState :: Double
                          , mJobName     :: String
                          , mJobStatus   :: Status
                          }
