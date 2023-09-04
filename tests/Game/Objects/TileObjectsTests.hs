module Game.Objects.TileObjectsTests where

import PotatoCactus.Game.Entity.Object.DynamicObject (DynamicObject (..), VisibleObject (Hidden, Visible))
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject), GameObjectType)
import PotatoCactus.Game.Entity.Object.TileObjects (TileObjects, addObject, create, findVisibleObjectById, objects, removeObject)
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

testTileObjects :: Test
testTileObjects =
  TestList
    [ TestCase
        ( assertEqual
            "adding a brand new object"
            [Added obj]
            ( create emptyStaticSet
                |> addObject obj
                |> objects
            )
        ),
      TestCase
        ( assertEqual
            "remove an added dynamic object"
            []
            ( create emptyStaticSet
                |> addObject obj
                |> removeObject 10
                |> objects
            )
        ),
      TestCase
        ( assertEqual
            "remove an object from the static set"
            [Removed staticObj]
            ( create populatedStaticSet
                |> removeObject 10
                |> objects
            )
        ),
      TestCase
        ( assertEqual
            "removing a non-existent static object should do nothing"
            []
            ( create emptyStaticSet
                |> removeObject 10
                |> objects
            )
        ),
      TestCase
        ( assertEqual
            "replace an object of the static set"
            [Replacing replacingObj staticObj]
            ( create populatedStaticSet
                |> addObject replacingObj
                |> objects
            )
        ),
      TestCase
        ( assertEqual
            "re-adding a dynamically removed object from the static set should bring back to initial state"
            []
            ( create populatedStaticSet
                |> removeObject 10
                |> addObject staticObj
                |> objects
            )
        ),
      TestCase
        ( assertEqual
            "adding a different object over a removed static set element yields a Replacing"
            [Replacing replacingObj staticObj]
            ( create populatedStaticSet
                |> removeObject 10
                |> addObject replacingObj
                |> objects
            )
        ),
      TestCase
        ( assertEqual
            "findVisibleObjectById yields reference to added object"
            (Visible obj)
            ( create emptyStaticSet
                |> addObject obj
                |> findVisibleObjectById 1
            )
        ),
      TestCase
        ( assertEqual
            "findVisibleObjectById yields reference to replaced object"
            (Visible obj)
            ( create populatedStaticSet
                |> addObject obj
                |> findVisibleObjectById 1
            )
        ),
      TestCase
        ( assertEqual
            "findVisibleObjectById yields reference to removed object"
            Hidden
            ( create populatedStaticSet
                |> removeObject 10
                |> findVisibleObjectById 2
            )
        )
    ]

pos :: Position
pos = Position 100 100 0

obj :: GameObject
obj = GameObject 1 pos 10 0

staticObj :: GameObject
staticObj = GameObject 2 pos 10 0

replacingObj :: GameObject
replacingObj = GameObject 3 pos 10 0

emptyStaticSet :: GameObjectType -> Maybe GameObject
emptyStaticSet _ = Nothing

populatedStaticSet :: GameObjectType -> Maybe GameObject
populatedStaticSet _ = Just staticObj
