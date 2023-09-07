module Main where

import Base37Test (testDecode, testEncode)
import BinaryTest (testByte, testByteNegate, testIntME, testMixedBitMode, testPack, testShortAdd, testShortBE, testShortLE)
import DecodeChatTests (decodeChatTests, encodeChatTests, testNibbles)
import Game.DynamicObjectCollectionTests (testDynamicObjectCollection)
import Game.GameObjectUpdateDiffTests (testObjectDiff)
import Game.GameObjectUpdateOperationsTests (testGameObjectUpdateOperations)
import Game.GroudItemCollectionTests (testGroundItemCollection)
import Game.GroundItemsUpdateDiffTests (groundItemsUpdateDiffTests)
import Game.InterpolatePathTests (interpolatePathTests)
import Game.Objects.TileObjectsTests (testTileObjects)
import Game.Player.AdvancePlayerTests (advancePlayerTests)
import Game.Player.InteractionTests (advanceInteractionTests)
import Game.Player.VarpSetTests (testVarpSet)
import Game.World.SelectorsTests (worldSelectorsTest)
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
      groundItemsUpdateDiffTests,
      testVarpSet,
      testDynamicObjectCollection,
      testTileObjects,
      testGameObjectUpdateOperations,
      worldSelectorsTest,
      advanceInteractionTests,
      advancePlayerTests
    ]

main :: IO ()
main = runTestTTAndExit tests
