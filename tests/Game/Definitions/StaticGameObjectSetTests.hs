module Game.Definitions.StaticGameObjectSetTests where

import PotatoCactus.Game.Definitions.StaticGameObjectSet (StaticGameObjectSet, createStaticObjectSet, objectsInRegion)
import qualified PotatoCactus.Game.Definitions.StaticGameObjectSet as StaticObjectSet
import qualified PotatoCactus.Game.Entity.Object.GameObject as GameObject
import PotatoCactus.Game.Movement.Pathing.TileFlagsUtils (mapChunkKey, mapRegionKey)
import qualified PotatoCactus.Game.Position as Pos
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

staticGameObjectSetTests :: Test
staticGameObjectSetTests =
  TestList
    [ TestCase
        ( assertEqual
            "looks up objects per chunk"
            [staticObj]
            (objectsInRegion staticSet (mapChunkKey staticObjectPos))
        )
    ]

staticObjectPos :: Pos.Position
staticObjectPos = Pos.Position 200 200 0

staticObj :: GameObject.GameObject
staticObj = GameObject.GameObject 2 staticObjectPos 10 0

staticSet :: StaticGameObjectSet
staticSet =
  createStaticObjectSet
    |> StaticObjectSet.addObject_ staticObj
