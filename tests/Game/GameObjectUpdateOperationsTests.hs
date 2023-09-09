module Game.GameObjectUpdateOperationsTests where

import qualified PotatoCactus.Client.GameObjectUpdate.GameObjectUpdateDiff as Diff
import PotatoCactus.Client.GameObjectUpdate.GameObjectUpdateOperations (OpType (AddObject, RemoveObject), selectOperations)
import qualified PotatoCactus.Game.Entity.Object.DynamicObject as Object
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as Object
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Position (Position (Position))
import Test.HUnit

testGameObjectUpdateOperations :: Test
testGameObjectUpdateOperations =
  TestList
    [ TestCase
        ( assertEqual
            "maps new added object to an AddObject operation"
            [AddObject $ mockObject added]
            (selectOperations [Diff.Added mockAddedObject])
        ),
      TestCase
        ( assertEqual
            "maps new removed object to a RemoveObject operation"
            [RemoveObject $ mockObject removed]
            (selectOperations [Diff.Added mockRemovedObject])
        ),
      TestCase
        ( assertEqual
            "does not issue remove operations for replaced objects"
            [AddObject $ mockObject added]
            (selectOperations [Diff.Added mockReplacedObject])
        ),
      TestCase
        ( assertEqual
            "reverts an Added object by removing the dynamic object"
            [RemoveObject $ mockObject added]
            (selectOperations [Diff.Removed mockAddedObject])
        ),
      TestCase
        ( assertEqual
            "reverts a removal by adding back the static object"
            [AddObject $ mockObject removed]
            (selectOperations [Diff.Removed mockRemovedObject])
        ),
      TestCase
        ( assertEqual
            "reverts a replaced object by adding back the static object"
            [AddObject $ mockObject removed]
            (selectOperations [Diff.Removed mockReplacedObject])
        ),
      TestCase
        ( assertEqual
            "issues at most one operation per tile/type combination"
            [AddObject $ mockObject 1]
            ( selectOperations
                [ Diff.Added . Object.Added $ mockObject 1,
                  Diff.Removed . Object.Added $ mockObject 2
                ]
            )
        )
    ]

added = 1234

removed = 4321

pos = Position 0 0 0

objType = 10

mockObject :: Int -> GameObject
mockObject id = GameObject id pos objType 0

mockAddedObject :: Object.DynamicObject
mockAddedObject = Object.Added $ mockObject added

mockRemovedObject :: Object.DynamicObject
mockRemovedObject = Object.Removed $ mockObject removed

mockReplacedObject :: Object.DynamicObject
mockReplacedObject = Object.Replacing (mockObject added) (mockObject removed)
