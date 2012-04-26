module Monitor where

import Job

data Monitor = JobMonitor { mJobId     :: String
                          , mJobName   :: String
                          , mJobStatus :: Status
                          }
