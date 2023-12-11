module Main where

import Base37Test (testDecode, testEncode)
import BinaryTest (testByte, testByteNegate, testIntME, testMixedBitMode, testPack, testShortAdd, testShortBE, testShortLE)
import DecodeChatTests (decodeChatTests, encodeChatTests, testNibbles)
import Game.ApplyActions.RemoveGroundItemActionTests (removeGroundItemActionTests)
import Game.Combat.AdvanceCombatEntityTests (advanceCombatEntityTests)
import Game.Combat.LocateCombatTargetTests (locateCombatTargetTests)
import Game.Definitions.StaticGameObjectSetTests (staticGameObjectSetTests)
import Game.DynamicObjectCollectionTests (testDynamicObjectCollection)
import Game.Entity.Npc.NpcMovementTests (npcMovementTests)
import Game.GameObjectUpdateDiffTests (testObjectDiff)
import Game.GameObjectUpdateOperationsTests (testGameObjectUpdateOperations)
import Game.GroudItemCollectionTests (testGroundItemCollection)
import Game.GroundItemsUpdateDiffTests (groundItemsUpdateDiffTests)
import Game.InterpolatePathTests (interpolatePathTests)
import Game.Movement.Pathing.CollisionMapBuilderTests (collisionMapBuilderTests)
import Game.Movement.Pathing.CollisionMapTests (collisionMapTests)
import Game.Movement.Pathing.PathPlannerTests (pathPlannerTests)
import Game.Movement.Pathing.TileFlagsMapTests (tileFlagsMapTests)
import Game.Objects.TileObjectsTests (testTileObjects)
import Game.Player.AdvancePlayerTests (advancePlayerTests)
import Game.Player.CanInteractWithEntityTests (canInteractWithEntityTests)
import Game.Player.ClosestInteractableTileCalcTests (closestInteractableTileCalcTests)
import Game.Player.InteractionTests (advanceInteractionTests)
import Game.Player.LocateInteractionTargetTests (locateInteractionTargetTests)
import Game.Player.PlayerMovementTests (playerMovementTests)
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
      advancePlayerTests,
      locateInteractionTargetTests,
      closestInteractableTileCalcTests,
      canInteractWithEntityTests,
      removeGroundItemActionTests,
      locateCombatTargetTests,
      advanceCombatEntityTests,
      npcMovementTests,
      playerMovementTests,
      tileFlagsMapTests,
      collisionMapTests,
      pathPlannerTests,
      staticGameObjectSetTests,
      collisionMapBuilderTests
    ]

main :: IO ()
main = runTestTTAndExit tests
