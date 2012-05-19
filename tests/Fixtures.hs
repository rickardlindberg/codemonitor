module Fixtures where

import Asserts
import Control.Concurrent
import Control.Exception.Base (bracket)
import Data.IORef
import Notifier
import Render.Rect
import System.Directory
import System.IO
import Test.HUnit
import Test.QuickCheck

setupNotificationsTest :: FilePath -> IO (FilePath -> Assertion)
setupNotificationsTest dir = do
    notificationsRef <- newIORef []
    setupNotifications dir (\f -> modifyIORef notificationsRef (f:))
    return $ \file -> do
        threadDelay 10000
        files <- readIORef notificationsRef
        file `assertElem` files

withTmpDir :: (FilePath -> IO a) -> IO a
withTmpDir = bracket setUp tearDown
    where
        tmpDir   = "/tmp/codemonitor-test"
        setUp    = createDirectory tmpDir >> return tmpDir
        tearDown = removeDirectoryRecursive

modifyFile :: FilePath -> IO ()
modifyFile path = do
    h <- openFile path AppendMode
    hPutStrLn h "a new line"
    hClose h

instance Arbitrary Rect where
    arbitrary = do
        x <- choose (0, 100)
        y <- choose (0, 100)
        w <- choose (0, 100)
        h <- choose (0, 100)
        return (Rect x y w h)
