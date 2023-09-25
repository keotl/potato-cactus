module Game.Movement.Pathing.CollisionMapTests where

import PotatoCactus.Game.Movement.Pathing.CollisionMap (CollisionMap, markSolidOccupant, markFlatWall)
import PotatoCactus.Game.Movement.Pathing.MovementFlags (allowsFullMovement, blocksAllMovement, blocksMovementSE, blocksMovementS, blocksMovementSW, blocksMovementNW, blocksMovementN, blocksMovementNE)
import qualified PotatoCactus.Game.Movement.Pathing.TileFlagsMap as TileFlagsMap
import PotatoCactus.Game.Position (Position (Position), x, y)
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit
import Data.Bits ((.|.))

collisionMapTests :: Test
collisionMapTests =
  TestList
    [ TestCase
        ( assertEqual
            "marks solid object"
            blocksAllMovement
            ( collisionMap
                |> markSolidOccupant (pos, (1, 1), 0)
                |> TileFlagsMap.getTileFlags pos
            )
        ),
      TestCase
        ( assertEqual
            "marks all tiles of a large solid object"
            [allowsFullMovement, blocksAllMovement, blocksAllMovement, allowsFullMovement]
            ( collisionMap
                |> markSolidOccupant (pos, (1, 2), 0)
                |> ( \colMap ->
                       map
                         (`TileFlagsMap.getTileFlags` colMap)
                         [pos {y = 99}, pos, pos {y = 101}, pos {y = 102}]
                   )
            )
        ),
      TestCase
        ( assertEqual
            "flips the object width and height when facing direction is 1 or 3"
            [allowsFullMovement, blocksAllMovement, blocksAllMovement, allowsFullMovement]
            ( collisionMap
                |> markSolidOccupant (pos, (1, 2), 3)
                |> ( \colMap ->
                       map
                         (`TileFlagsMap.getTileFlags` colMap)
                         [pos {x = 99}, pos, pos {x = 101}, pos {x = 102}]
                   )
            )
        ),
      TestCase
        ( assertEqual
            "marks flat wall tile blocking direction and diagonals"
            -- \ x /   tiles blocked by wall
            -- . - .   wall facing N
            [
              {- first row -}  blocksMovementSE, blocksMovementSW .|. blocksMovementS .|. blocksMovementSE, blocksMovementSW,
              {- second row -} blocksMovementNE, blocksMovementNW .|. blocksMovementN .|. blocksMovementNE, blocksMovementNW
            ]
            ( collisionMap
                |> markFlatWall pos 1
                |> ( \colMap ->
                       map
                         (`TileFlagsMap.getTileFlags` colMap)
                         [
                           pos {x = 99, y = 101}, pos { y = 101}, pos {x = 101, y = 101},
                           pos {x = 99}, pos, pos {x = 101}
                         ]
                   )
            )
        )
    ]

pos :: Position
pos = Position 100 100 0

collisionMap :: CollisionMap
collisionMap = TileFlagsMap.create
