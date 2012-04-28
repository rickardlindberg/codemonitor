import Notifier

import Control.Concurrent
import Control.Exception.Base (bracket)
import Data.IORef
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import Test.HUnit

tests = test
    [ "file modified in directory" ~: withTmpDir $ \dir -> do
        filesRef <- newIORef []
        setupNotifications dir (\f -> modifyIORef filesRef (f:))

        touch $ dir </> "foo"
        assertGotFile filesRef "foo"

    , "file modified in subdirectory" ~: withTmpDir $ \dir -> do
        createDirectory $ dir </> "subdir"

        filesRef <- newIORef []
        setupNotifications dir (\f -> modifyIORef filesRef (f:))

        touch $ dir </> "subdir" </> "foo"
        assertGotFile filesRef "foo"
    ]

withTmpDir :: (FilePath -> IO a) -> IO a
withTmpDir = bracket setUp tearDown
    where
        tmpDir   = "/tmp/codemonitor-test"
        setUp    = createDirectory tmpDir >> return tmpDir
        tearDown = removeDirectoryRecursive

touch :: FilePath -> IO ()
touch path = openFile path WriteMode >>= hClose

assertGotFile :: IORef [String] -> String -> Assertion
assertGotFile filesRef file = do
    threadDelay 10000
    files <- readIORef filesRef
    file `assertElem` files

assertElem :: String -> [String] -> Assertion
assertElem item list =
    assertBool ("expected item '" ++ item ++ "' to be in " ++ show list)
               (item `elem` list)

main = runTestTT tests >>= exit
    where
        exit Counts { errors=0, failures=0 } = exitSuccess
        exit _                               = exitFailure
