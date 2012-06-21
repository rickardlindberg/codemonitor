module Job.Types where

import Text.Regex.Posix

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

fullName :: JobDescription -> String
fullName (JobDescription { name = name, args = args }) = name ++ " " ++ unwords args
