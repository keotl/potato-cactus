module PotatoCactus.Game.Movement.Pathing.CollisionMap (CollisionMap, markSolidOccupant, markSolidTile, markFlatWall) where

import Data.Bits ((.|.))
import PotatoCactus.Game.Movement.Pathing.MovementFlags (blocksAllMovement, blocksMovementE, blocksMovementN, blocksMovementNE, blocksMovementNW, blocksMovementS, blocksMovementSE, blocksMovementSW, blocksMovementW)
import qualified PotatoCactus.Game.Movement.Pathing.TileFlagsMap as TileFlagsMap
import PotatoCactus.Game.Position (Position (Position, x, y, z))
import PotatoCactus.Utils.Flow ((|>))

type CollisionMap = TileFlagsMap.TileFlagsMap

markSolidOccupant :: (Position, (Int, Int), Int) -> CollisionMap -> CollisionMap
markSolidOccupant obj collisionMap =
  foldl
    (flip markSolidTile)
    collisionMap
    (occupiedTiles obj)

occupiedTiles :: (Position, (Int, Int), Int) -> [Position]
occupiedTiles (pos, dimensions, facing) =
  [Position i j (z pos) | i <- xRange pos dimensions facing, j <- yRange pos dimensions facing]

xRange :: Position -> (Int, Int) -> Int -> [Int]
xRange originPos (width, height) 0 = [x originPos .. x originPos + width - 1]
xRange originPos (width, height) 1 = [x originPos .. x originPos + height - 1]
xRange originPos (width, height) facing = xRange originPos (width, height) (facing `mod` 2)

yRange :: Position -> (Int, Int) -> Int -> [Int]
yRange originPos (width, height) 0 = [y originPos .. y originPos + height - 1]
yRange originPos (width, height) 1 = [y originPos .. y originPos + width - 1]
yRange originPos (width, height) facing = yRange originPos (width, height) (facing `mod` 2)

markSolidTile :: Position -> CollisionMap -> CollisionMap
markSolidTile = TileFlagsMap.setTileFlags blocksAllMovement

-- Flat Wall i.e. objType = 0
markFlatWall :: Position -> Int -> CollisionMap -> CollisionMap
markFlatWall pos 0 collisionMap =
  -- Wall on W edge
  collisionMap
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementW) pos
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementE) pos {x = x pos - 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSW) pos
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNE) pos {x = x pos - 1, y = y pos - 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNW) pos
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSE) pos {x = x pos - 1, y = y pos + 1}
    -- reciprocal diagonals
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSE) pos {x = x pos - 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNW) pos {y = y pos - 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNE) pos {x = x pos - 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSW) pos {y = y pos + 1}
markFlatWall pos 1 collisionMap =
  -- Wall on N edge
  collisionMap
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementN) pos
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementS) pos {y = y pos + 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNW) pos
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSE) pos {x = x pos - 1, y = y pos + 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNE) pos
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSW) pos {x = x pos + 1, y = y pos + 1}
    -- reciprocal diagonals
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSW) pos {y = y pos + 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNE) pos {x = x pos - 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSE) pos {y = y pos + 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNW) pos {x = x pos + 1}
markFlatWall pos 2 collisionMap =
  -- Wall on E edge
  collisionMap
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementE) pos
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementW) pos {x = x pos + 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNE) pos
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSW) pos {x = x pos + 1, y = y pos + 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSE) pos
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNW) pos {x = x pos + 1, y = y pos - 1}
    -- reciprocal diagonals
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNW) pos {x = x pos + 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSE) pos {y = y pos + 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSW) pos {x = x pos + 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNE) pos {y = y pos - 1}
markFlatWall pos 3 collisionMap =
  -- Wall on S edge
  collisionMap
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementS) pos
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementN) pos {y = y pos - 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSE) pos
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNW) pos {x = x pos + 1, y = y pos - 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSW) pos
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNE) pos {x = x pos - 1, y = y pos - 1}
    -- reciprocal diagonals
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNE) pos {y = y pos - 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSW) pos {x = x pos + 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementNW) pos {y = y pos - 1}
    |> TileFlagsMap.alterTileFlags (.|. blocksMovementSE) pos {x = x pos - 1}
markFlatWall _ _ c = c
