module Game.DynamicObjectCollectionTests where

import Debug.Trace (trace)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject (Added, Removed), DynamicObjectCollection)
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as ObjectCollection
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject), GameObjectType)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position), chunkX, chunkY)
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

testDynamicObjectCollection =
  TestList
    [ TestCase
        ( assertEqual
            "add brand new object should create Added dynamic object"
            [Added obj]
            (ObjectCollection.iter (ObjectCollection.addDynamicObject dummyStaticSetLookup obj ObjectCollection.create))
        ),
      TestCase
        ( assertEqual
            "remove Added object"
            []
            ( ObjectCollection.iter $
                ObjectCollection.removeDynamicObject dummyStaticSetLookup obj collection
            )
        ),
      TestCase
        ( assertEqual
            "removing an object not in the static set should do nothing"
            []
            ( ObjectCollection.iter $
                ObjectCollection.removeDynamicObject dummyStaticSetLookup obj ObjectCollection.create
            )
        ),
      TestCase
        ( assertEqual
            "add over another dynamic object replaces in-place"
            [Added obj]
            ( ObjectCollection.iter
                ( ObjectCollection.create
                    |> ObjectCollection.addDynamicObject dummyStaticSetLookup staticObj
                    |> ObjectCollection.addDynamicObject dummyStaticSetLookup obj
                )
            )
        ),
      TestCase
        ( assertEqual
            "adding over Removed object should remove that entry"
            []
            ( ObjectCollection.iter
                ( ObjectCollection.removeDynamicObject
                    (\_ _ -> [obj])
                    obj
                    ObjectCollection.create
                    |> ObjectCollection.addDynamicObject dummyStaticSetLookup obj
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
            "addDynamicObject with replaced object in static set, should mark static object as removed"
            [Added obj, Removed staticObj]
            ( ObjectCollection.iter
                ( ObjectCollection.create
                    |> ObjectCollection.addDynamicObject (\_ _ -> [staticObj]) obj
                )
            )
        ),
      TestCase
        ( assertEqual
            "re-adding a replaced static object resets to initial state"
            []
            ( ObjectCollection.iter
                ( ObjectCollection.create
                    |> ObjectCollection.addDynamicObject (\_ _ -> [staticObj]) obj
                    |> ObjectCollection.addDynamicObject (\_ _ -> [staticObj]) staticObj
                )
            )
        ),
      TestCase
        ( assertEqual
            "addDynamicObject with replaced object in static set AND existing chunkmap, should mark static object as removed"
            [Added obj, Removed staticObj]
            ( ObjectCollection.iter
                ( ObjectCollection.create
                    |> ObjectCollection.addDynamicObject dummyStaticSetLookup obj
                    |> ObjectCollection.removeDynamicObject dummyStaticSetLookup obj
                    |> ObjectCollection.addDynamicObject (\_ _ -> [staticObj]) obj
                )
            )
        ),
      TestCase
        ( assertEqual
            "removeDynamicObject on an Added object replacing a static object, should remove both the Added and Removed objects"
            []
            ( ObjectCollection.iter
                ( ObjectCollection.create
                    |> ObjectCollection.addDynamicObject (\_ _ -> [staticObj]) obj
                    |> ObjectCollection.removeDynamicObject (\_ _ -> [staticObj]) obj
                )
            )
        )
    ]

collection :: DynamicObjectCollection
collection =
  foldl
    (flip (ObjectCollection.addDynamicObject dummyStaticSetLookup))
    ObjectCollection.create
    [obj]

obj :: GameObject
obj = GameObject 123 (Position 3167 3304 0) 0 0

staticObj :: GameObject
staticObj = GameObject 456 (Position 3167 3304 0) 0 0

dummyStaticSetLookup :: Position -> GameObjectType -> [GameObject]
dummyStaticSetLookup _ _ = []
