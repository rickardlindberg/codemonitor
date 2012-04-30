module Asserts where

import Control.Concurrent
import Data.IORef
import Test.HUnit

assertGotFile :: IORef [String] -> String -> Assertion
assertGotFile filesRef file = do
    threadDelay 10000
    files <- readIORef filesRef
    file `assertElem` files

assertElem :: String -> [String] -> Assertion
assertElem item list =
    assertBool ("expected item '" ++ item ++ "' to be in " ++ show list)
               (item `elem` list)
