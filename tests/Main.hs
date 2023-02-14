module Main where

import Base37Test (testDecode, testEncode)
import BinaryTest (testByte, testByteNegate, testIntME, testMixedBitMode, testPack, testShortBE, testShortLE, testShortAdd)
import GetMonadTests (getTests)
import Test.HUnit
import Game.InterpolatePathTests (interpolatePathTests)
import DecodeChatTests (decodeChatTests, testNibbles, encodeChatTests)

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
      encodeChatTests,
      testNibbles
    ]

main :: IO ()
main = runTestTTAndExit tests
