module Fixtures where

import Control.Exception.Base (bracket)
import Rect
import System.Directory
import System.IO
import Test.QuickCheck

withTmpDir :: (FilePath -> IO a) -> IO a
withTmpDir = bracket setUp tearDown
    where
        tmpDir   = "/tmp/codemonitor-test"
        setUp    = createDirectory tmpDir >> return tmpDir
        tearDown = removeDirectoryRecursive

touch :: FilePath -> IO ()
touch path = openFile path WriteMode >>= hClose

instance Arbitrary Rect where
    arbitrary = do
        x <- choose (0, 100)
        y <- choose (0, 100)
        w <- choose (0, 100)
        h <- choose (0, 100)
        return (Rect x y w h)
