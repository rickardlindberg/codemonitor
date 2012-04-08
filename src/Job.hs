module Job where

import System.Process

data Job = Job String [String] (Maybe ProcessHandle)

name :: Job -> String
name (Job name args _) = name
