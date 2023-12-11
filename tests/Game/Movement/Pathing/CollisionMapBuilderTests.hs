module Game.Movement.Pathing.CollisionMapBuilderTests where

import Debug.Trace (trace)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Movement.Pathing.CollisionMap (CollisionMap, markSurroundingRegionDirty)
import qualified PotatoCactus.Game.Movement.Pathing.CollisionMap as CollisionMap
import PotatoCactus.Game.Movement.Pathing.CollisionMapBuilder (buildCollisionMap, recomputeDirtyRegions)
import PotatoCactus.Game.Movement.Pathing.MovementFlags (blocksAllMovement)
import qualified PotatoCactus.Game.Movement.Pathing.TileFlagsMap as TileFlags
import PotatoCactus.Game.Movement.Pathing.TileFlagsUtils (mapChunkKey)
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

collisionMapBuilderTests :: Test
collisionMapBuilderTests =
  TestList
    [ TestCase
        ( assertEqual
            "builds a collision map for objects"
            blocksAllMovement
            ( buildCollisionMap [obj]
                |> CollisionMap.tileFlags
                |> TileFlags.getTileFlags pos
            )
        ),
      TestCase
        ( assertEqual
            "recomputes dirty regions"
            collisionMap
            ( CollisionMap.create
                |> markSurroundingRegionDirty pos
                |> recomputeDirtyRegions (\key -> [obj | key == mapChunkKey pos])
            )
        )
    ]

pos :: Position
pos = Position 3167 3304 0

obj :: GameObject
obj = GameObject 123 pos 10 0

collisionMap :: CollisionMap
collisionMap = buildCollisionMap [obj]
