module Main where

import Base37Test (testDecode, testEncode)
import BinaryTest (testByte, testByteNegate, testIntME, testMixedBitMode, testPack, testShortBE, testShortLE)
import GetMonadTests (getTests)
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
      testPack,
      getTests
    ]

main :: IO ()
main = runTestTTAndExit tests
