module Main where

import Base37Test (testDecode, testEncode)
import Test.HUnit

tests =
  TestList
    [testDecode,testEncode]

main :: IO ()
main = runTestTTAndExit tests
