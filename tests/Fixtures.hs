module Fixtures where

import Control.Exception.Base (bracket)
import System.Directory
import System.IO

withTmpDir :: (FilePath -> IO a) -> IO a
withTmpDir = bracket setUp tearDown
    where
        tmpDir   = "/tmp/codemonitor-test"
        setUp    = createDirectory tmpDir >> return tmpDir
        tearDown = removeDirectoryRecursive

touch :: FilePath -> IO ()
touch path = openFile path WriteMode >>= hClose
