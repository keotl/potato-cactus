module Game.DynamicObjectCollectionTests where

import PotatoCactus.Game.Entity.Object.DynamicObject (DynamicObject (..))
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObjectCollection)
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as ObjectCollection
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject, objectType, position), GameObjectType)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position), chunkX, chunkY)
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

testDynamicObjectCollection =
  TestList
    [ TestCase
        ( assertEqual
            "add brand new object should create Added dynamic object"
            [Added obj]
            (ObjectCollection.iter (ObjectCollection.addDynamicObject obj emptyCollection))
        ),
      TestCase
        ( assertEqual
            "remove Added object"
            []
            ( ObjectCollection.iter $
                ObjectCollection.removeDynamicObject (position obj, objectType obj) collection
            )
        ),
      TestCase
        ( assertEqual
            "removing an object not in the static set should do nothing"
            []
            ( ObjectCollection.iter $
                ObjectCollection.removeDynamicObject (position obj, objectType obj) emptyCollection
            )
        ),
      TestCase
        ( assertEqual
            "add over another dynamic object replaces in-place"
            [Added obj]
            ( ObjectCollection.iter
                ( emptyCollection
                    |> ObjectCollection.addDynamicObject staticObj
                    |> ObjectCollection.addDynamicObject obj
                )
            )
        ),
      TestCase
        ( assertEqual
            "adding over Removed object should remove that entry"
            []
            ( ObjectCollection.iter
                ( ObjectCollection.create populatedStaticSet
                    |> ObjectCollection.removeDynamicObject (position staticObj, objectType staticObj)
                    |> ObjectCollection.addDynamicObject staticObj
                )
            )
        ),
      TestCase
        ( assertEqual
            "findByChunkXY"
            [Added obj]
            (ObjectCollection.findByChunkXY (chunkX . getPosition $ obj) (chunkY . getPosition $ obj) 0 collection)
        ),
      TestCase
        ( assertEqual
            "addDynamicObject with replaced object in static set, should mark static object as Replaced"
            [Replacing obj staticObj]
            ( ObjectCollection.iter
                ( ObjectCollection.create populatedStaticSet
                    |> ObjectCollection.addDynamicObject obj
                )
            )
        ),
      TestCase
        ( assertEqual
            "re-adding a replaced static object resets to initial state"
            []
            ( ObjectCollection.iter
                ( ObjectCollection.create populatedStaticSet
                    |> ObjectCollection.addDynamicObject obj
                    |> ObjectCollection.addDynamicObject staticObj
                )
            )
        ),
      TestCase
        ( assertEqual
            "removeDynamicObject on an Replacing object should leave static object as Removed"
            -- TODO - What is the most reasonable intended behaviour here?  - keotl 2023-08-31
            -- Either revert to default state (i.e. static object) or with static object removed
            [Removed staticObj]
            ( ObjectCollection.iter
                ( ObjectCollection.create populatedStaticSet
                    |> ObjectCollection.addDynamicObject obj
                    |> ObjectCollection.removeDynamicObject (position obj, objectType obj)
                )
            )
        )
    ]

emptyCollection :: DynamicObjectCollection
emptyCollection = ObjectCollection.create emptyStaticSet

collection :: DynamicObjectCollection
collection =
  foldl
    (flip ObjectCollection.addDynamicObject)
    emptyCollection
    [obj]

obj :: GameObject
obj = GameObject 123 (Position 3167 3304 0) 0 0

staticObj :: GameObject
staticObj = GameObject 456 (Position 3167 3304 0) 0 0

emptyStaticSet :: Position -> GameObjectType -> Maybe GameObject
emptyStaticSet _ _ = Nothing

populatedStaticSet :: Position -> GameObjectType -> Maybe GameObject
populatedStaticSet _ _ = Just staticObj
