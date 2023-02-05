module Main where

import Base37Test (testDecode, testEncode)
import Test.HUnit
import BinaryTest (testShortLE, testShortBE, testByte, testIntME)

tests =
  TestList
    [testDecode,testEncode,
    testShortLE, testShortBE, testByte, testIntME]

main :: IO ()
main = runTestTTAndExit tests
