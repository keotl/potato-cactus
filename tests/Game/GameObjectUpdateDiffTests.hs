module Game.GameObjectUpdateDiffTests where

import PotatoCactus.Client.GameObjectUpdate.GameObjectUpdateDiff (GameObjectDiff (Added, Removed), computeDiff)
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as Object
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import qualified PotatoCactus.Game.Entity.Object.GameObject as Object
import PotatoCactus.Game.Position (Position (Position))
import Test.HUnit

testObjectDiff :: Test
testObjectDiff =
  TestList
    [ TestCase
        ( assertEqual
            "replacing existing object should not yield Removed message"
            [Added $ mockObject_ 1234]
            (computeDiff [mockObject_ 4321] [mockObject_ 1234])
        ),
      TestCase
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
        )
    ]

mockObject_ :: Int -> Object.DynamicObject
mockObject_ objectId =
  Object.Added $ Object.GameObject objectId (Position 100 100 0) 10 0
