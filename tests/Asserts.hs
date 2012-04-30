module Asserts where

import Test.HUnit

assertElem :: String -> [String] -> Assertion
assertElem item list =
    assertBool ("expected item '" ++ item ++ "' to be in " ++ show list)
               (item `elem` list)
