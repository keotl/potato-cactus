module PotatoCactus.Game.Movement.Pathing.CollisionMap (CollisionMap, create, markSolidOccupant) where

import PotatoCactus.Game.Movement.Pathing.MovementFlags (blocksAllMovement)
import qualified PotatoCactus.Game.Movement.Pathing.TileFlagsMap as TileFlagsMap
import PotatoCactus.Game.Position (Position (Position, x, y, z))

data CollisionMap = CollisionMap
  { tiles_ :: TileFlagsMap.TileFlagsMap
  }

create :: CollisionMap
create = CollisionMap TileFlagsMap.create

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
markSolidTile pos CollisionMap {tiles_ = tiles} =
  CollisionMap {tiles_ = TileFlagsMap.setTileFlags blocksAllMovement pos tiles}

