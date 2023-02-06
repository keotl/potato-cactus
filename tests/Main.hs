module Main where

import Base37Test (testDecode, testEncode)
import BinaryTest (testByte, testByteNegate, testIntME, testMixedBitMode, testShortBE, testShortLE, testPack)
import Test.HUnit

tests =
  TestList
    [ testDecode,
      testEncode,
      testShortLE,
      testShortBE,
      testByte,
      testIntME,
      testByteNegate,
      testMixedBitMode,
      testPack
    ]

main :: IO ()
main = runTestTTAndExit tests
