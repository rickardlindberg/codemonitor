module Job.Types where

import Control.Concurrent

data Status = Idle
            | Working
            | Fail
            deriving (Eq)

data Job = Job
    { jobId     :: String
    , name      :: String
    , args      :: [String]
    , matchExpr :: String
    , status    :: Status
    , output    :: String
    , thread    :: Maybe ThreadId
    }

data Jobs = Jobs [Job]

createJob :: String -> String -> [String] -> String -> Job
createJob jobId name args expr = Job jobId name args expr Idle "" Nothing

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
