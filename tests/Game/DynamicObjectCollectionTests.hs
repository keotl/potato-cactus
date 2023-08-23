module Game.DynamicObjectCollectionTests where

import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject (Added), DynamicObjectCollection)
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as ObjectCollection
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position), chunkX, chunkY)
import Test.HUnit

testDynamicObjectCollection =
  TestList
    [ TestCase
        ( assertEqual
            "findByChunkXY"
            [Added obj]
            (ObjectCollection.findByChunkXY (chunkX . getPosition $ obj) (chunkY . getPosition $ obj) 0 collection)
        ),
      TestCase
        ( assertEqual
            "removeDynamicObject"
            []
            ( ObjectCollection.iter $
                ObjectCollection.removeDynamicObject (getPosition obj, 0) collection
            )
        )
    ]

collection :: DynamicObjectCollection
collection =
  foldl
    (flip ObjectCollection.addDynamicObject)
    ObjectCollection.create
    [Added obj]

obj :: GameObject
obj = GameObject 123 (Position 3167 3304 0) 0 0
