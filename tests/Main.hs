module Main where

import Base37Test (testDecode, testEncode)
import BinaryTest (testByte, testByteNegate, testIntME, testMixedBitMode, testPack, testShortAdd, testShortBE, testShortLE)
import DecodeChatTests (decodeChatTests, encodeChatTests, testNibbles)
import Game.GameObjectUpdateDiffTests (testObjectDiff)
import Game.GroudItemCollectionTests (testGroundItemCollection)
import Game.GroundItemsUpdateDiffTests (groundItemsUpdateDiffTests)
import Game.InterpolatePathTests (interpolatePathTests)
import GetMonadTests (getTests)
import IterableTests (testReplaceAt)
import MobListTests (testMobList)
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
      testShortAdd,
      testMixedBitMode,
      testPack,
      getTests,
      interpolatePathTests,
      decodeChatTests,
      encodeChatTests,
      testNibbles,
      testReplaceAt,
      testMobList,
      testObjectDiff,
      testGroundItemCollection,
      groundItemsUpdateDiffTests
    ]

main :: IO ()
main = runTestTTAndExit tests
