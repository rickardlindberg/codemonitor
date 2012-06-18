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
    { runningId    :: String
    , jobStatus    :: Status
    , jobOutput    :: String
    , jobThread    :: Maybe ThreadId
    }

data RunningJobInfos = RunningJobInfos [RunningJobInfo]

createJob :: String -> String -> [String] -> String -> Job
createJob jobId name args expr = Job jobId name args expr (RunningJobInfo jobId Idle "" Nothing)

createJobs :: [Job] -> Jobs
createJobs = Jobs

jobWithId :: Jobs -> String -> Job
jobWithId (Jobs jobs) id = find jobs
    where
        find [] = error "gaaah"
        find (x:xs) | jobId x == id = x
                    | otherwise     = find xs

fullName :: Job -> String
fullName (Job { name = name, args = args }) = name ++ " " ++ unwords args
