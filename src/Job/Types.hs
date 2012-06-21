module Job.Types where

import Control.Concurrent
import Text.Regex.Posix

data JobStatus
    = Idle
    | Working
    | Fail
    deriving (Eq, Show)

data JobDescription = JobDescription
    { jobId     :: String
    , name      :: String
    , args      :: [String]
    , matchExpr :: String
    }

filterJobsMatching :: FilePath -> [JobDescription] -> [JobDescription]
filterJobsMatching fileChanged = filter isMatch
    where
        isMatch job = fileChanged =~ matchExpr job

data RunningJob = RunningJob
    { runningJobId :: String
    , jobStatus    :: JobStatus
    , jobOutput    :: String
    , jobThread    :: Maybe ThreadId
    }

mergeTwoInfos :: [RunningJob] -> [RunningJob] -> [RunningJob]
mergeTwoInfos new old =
    filter notInNew old ++ new
    where
        notInNew info =
            case runningJobWithId (runningJobId info) new of
                   Nothing -> True
                   Just x  -> False

runningJobWithId :: String -> [RunningJob] -> Maybe RunningJob
runningJobWithId id = find
    where
        find [] = Nothing
        find (x:xs)
            | runningJobId x == id = Just x
            | otherwise            = find xs

fullName :: JobDescription -> String
fullName (JobDescription { name = name, args = args }) = name ++ " " ++ unwords args
