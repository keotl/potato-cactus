module Main where

import Base37Test (testDecode, testEncode)
import Test.HUnit
import BinaryTest (testShortLE, testShortBE, testByte, testIntME, testByteNegate)

tests =
  TestList
    [testDecode,testEncode,
    testShortLE, testShortBE, testByte, testIntME, testByteNegate]

main :: IO ()
main = runTestTTAndExit tests
