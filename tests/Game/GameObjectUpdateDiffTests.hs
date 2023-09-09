module Game.GameObjectUpdateDiffTests where

import PotatoCactus.Client.GameObjectUpdate.GameObjectUpdateDiff (GameObjectDiff (Added, Removed, Retained), computeDiff)
import qualified PotatoCactus.Game.Entity.Object.DynamicObject as Object
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as Object
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Position (Position (Position))
import Test.HUnit

testObjectDiff :: Test
testObjectDiff =
  TestList
    [ TestCase
        ( assertEqual
            "remove objects"
            [Removed $ mockObject_ 4321]
            (computeDiff [mockObject_ 4321] [])
        ),
      TestCase
        ( assertEqual
            "adds objects"
            [Added $ mockObject_ 4321]
            (computeDiff [] [mockObject_ 4321])
        ),
      TestCase
        ( assertEqual
            "retains removed object"
            [Retained $ mockRemovedObject_ 4321]
            (computeDiff [mockRemovedObject_ 4321] [mockRemovedObject_ 4321])
        )
    ]

mockObject_ :: Int -> Object.DynamicObject
mockObject_ objectId =
  Object.Added $ GameObject objectId (Position 100 100 0) 10 0

mockRemovedObject_ :: Int -> Object.DynamicObject
mockRemovedObject_ objectId =
  Object.Removed $ GameObject objectId (Position 100 100 0) 10 0
