module Main where

import Base37Test (testDecode, testEncode)
import BinaryTest (testByte, testByteNegate, testIntME, testMixedBitMode, testPack, testShortBE, testShortLE, testShortAdd)
import GetMonadTests (getTests)
import Test.HUnit
import Game.InterpolatePathTests (interpolatePathTests)
import DecodeChatTests (decodeChatTests, testNibbles)

tests =
  TestList
    [ testDecode,
      testEncode,
      testShortLE,
      testShortBE,
      testByte,
      testIntME,
      testByteNegate,
      testShortAdd,
      testMixedBitMode,
      testPack,
      getTests,
      interpolatePathTests,
      decodeChatTests,
      testNibbles
    ]

main :: IO ()
main = runTestTTAndExit tests
