module Main where

import Base37Test (testDecode, testEncode)
import Test.HUnit
import BinaryTest (testShortLE, testShortBE)

tests =
  TestList
    [testDecode,testEncode,
    testShortLE, testShortBE]

main :: IO ()
main = runTestTTAndExit tests
