module Game.DynamicObjectCollectionTests where

import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject (Added, Removed), DynamicObjectCollection)
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as ObjectCollection
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position), chunkX, chunkY)
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

testDynamicObjectCollection =
  TestList
    [ TestCase
        ( assertEqual
            "add brand new object should create Added dynamic object"
            [Added obj]
            (ObjectCollection.iter (ObjectCollection.addDynamicObject obj ObjectCollection.create))
        ),
      TestCase
        ( assertEqual
            "remove Added object"
            []
            ( ObjectCollection.iter $
                ObjectCollection.removeDynamicObject obj collection
            )
        ),
      TestCase
        ( assertEqual
            "removing a brand new object should add as Removed"
            [Removed obj]
            ( ObjectCollection.iter $
                ObjectCollection.removeDynamicObject obj ObjectCollection.create
            )
        ),
      TestCase
        ( assertEqual
            "adding over Removed object should remove that entry"
            []
            ( ObjectCollection.iter
                ( ObjectCollection.removeDynamicObject
                    obj
                    ObjectCollection.create
                    |> ObjectCollection.addDynamicObject obj
                )
            )
        ),
      TestCase
        ( assertEqual
            "findByChunkXY"
            [Added obj]
            (ObjectCollection.findByChunkXY (chunkX . getPosition $ obj) (chunkY . getPosition $ obj) 0 collection)
        )
    ]

collection :: DynamicObjectCollection
collection =
  foldl
    (flip ObjectCollection.addDynamicObject)
    ObjectCollection.create
    [obj]

obj :: GameObject
obj = GameObject 123 (Position 3167 3304 0) 0 0
