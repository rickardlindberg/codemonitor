module Job.Description where

import Text.Regex.Posix

data JobDescription = JobDescription
    { jobId     :: String
    , name      :: String
    , args      :: [String]
    , matchExpr :: String
    }

filterJobsMatching :: [JobDescription] -> FilePath -> [JobDescription]
filterJobsMatching jobDescriptions fileChanged = filter isMatch jobDescriptions
    where
        isMatch job = fileChanged =~ matchExpr job

fullName :: JobDescription -> String
fullName (JobDescription { name = name, args = args }) = name ++ " " ++ unwords args
