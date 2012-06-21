module Job.Types where

import Control.Concurrent
import Data.Maybe

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

processRunningInfos :: (RunningJobInfo -> Maybe RunningJobInfo) -> RunningJobInfos -> RunningJobInfos
processRunningInfos fn (RunningJobInfos infos) = RunningJobInfos $ mapMaybe fn infos

mergeTwoInfos :: RunningJobInfos -> RunningJobInfos -> RunningJobInfos
mergeTwoInfos (RunningJobInfos new) (RunningJobInfos old) =
    RunningJobInfos $ filter notInNew old ++ new
    where
        notInNew info =
            case runningJobInfoWithId (RunningJobInfos new) (runningJobId info) of
                   Nothing -> True
                   Just x  -> False

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
