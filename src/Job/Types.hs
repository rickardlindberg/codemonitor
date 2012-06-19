module Job.Types where

import Control.Concurrent

data Status = Idle
            | Working
            | Fail
            deriving (Eq, Show)

data Job = Job
    { jobId     :: String
    , name      :: String
    , args      :: [String]
    , matchExpr :: String
    , runningInfo      :: RunningJobInfo
    }

data Jobs = Jobs [Job]

data RunningJobInfo = RunningJobInfo
    { runningJobId    :: String
    , jobStatus       :: Status
    , jobOutput       :: String
    , jobThread       :: Maybe ThreadId
    }

data RunningJobInfos = RunningJobInfos [RunningJobInfo]

createJob :: String -> String -> [String] -> String -> Job
createJob jobId name args expr = Job jobId name args expr (RunningJobInfo jobId Idle "" Nothing)

createJobs :: [Job] -> Jobs
createJobs = Jobs

jobsToRunningJobInfos :: Jobs -> RunningJobInfos
jobsToRunningJobInfos (Jobs jobs) = RunningJobInfos (map runningInfo jobs)

runningJobInfoWithId :: RunningJobInfos -> String -> Maybe RunningJobInfo
runningJobInfoWithId (RunningJobInfos runningInfos) id = find runningInfos
    where
        find [] = Nothing
        find (x:xs)
            | runningJobId x == id = Just x
            | otherwise            = find xs

fullName :: Job -> String
fullName (Job { name = name, args = args }) = name ++ " " ++ unwords args
