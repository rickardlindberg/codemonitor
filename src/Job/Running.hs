module Job.Running where

import Control.Concurrent

data JobStatus
    = Idle
    | Working
    | Fail
    deriving (Eq, Show)

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
