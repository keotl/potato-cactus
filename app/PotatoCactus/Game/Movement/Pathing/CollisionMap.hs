{-# LANGUAGE FlexibleInstances #-}

module PotatoCactus.Game.Movement.Pathing.CollisionMap (CollisionMap, markSolidOccupant, markSolidTile, markFlatWall, allowsMovementBetween, create, markSurroundingRegionDirty, alterForDirtyRegions, tileFlags) where

import Data.Binary (Word8)
import Data.Bits (Bits ((.&.)), (.|.))
import qualified Data.IntSet as IntSet
import qualified PotatoCactus.Game.Movement.Direction as Direction
import PotatoCactus.Game.Movement.Pathing.MovementFlags (blocksAllMovement, blocksMovementE, blocksMovementN, blocksMovementNE, blocksMovementNW, blocksMovementS, blocksMovementSE, blocksMovementSW, blocksMovementW)
import qualified PotatoCactus.Game.Movement.Pathing.TileFlagsMap as TileFlagsMap
import PotatoCactus.Game.Movement.Pathing.TileFlagsUtils (mapChunkKey)
import PotatoCactus.Game.Movement.PositionXY (toXY)
import PotatoCactus.Game.Position (Position (Position, x, y, z))
import PotatoCactus.Utils.Flow ((|>))

type RegionKey = Int

data CollisionMap = CollisionMap
  { tileFlags :: TileFlagsMap.TileFlagsMap,
    dirtyRegions :: IntSet.IntSet,
    regionKey_ :: Position -> RegionKey
  }
  deriving (Show, Eq)

instance Show (Position -> RegionKey) where
  show x = "<fn>"

instance Eq (Position -> RegionKey) where
  a == b = True

create :: CollisionMap
create = CollisionMap TileFlagsMap.create IntSet.empty mapChunkKey

markSurroundingRegionDirty :: Position -> CollisionMap -> CollisionMap
markSurroundingRegionDirty pos collisionMap =
  collisionMap
    { dirtyRegions =
        IntSet.insert
          (regionKey_ collisionMap pos)
          (dirtyRegions collisionMap)
    }

alterForDirtyRegions :: ([RegionKey] -> CollisionMap -> CollisionMap) -> CollisionMap -> CollisionMap
alterForDirtyRegions transform collisionMap =
  let dirty = dirtyRegions collisionMap
   in transform (IntSet.toList dirty) (resetDirtyRegions collisionMap)

resetDirtyRegions :: CollisionMap -> CollisionMap
resetDirtyRegions collisionMap =
  collisionMap
    { tileFlags =
        foldl
          TileFlagsMap.resetRegion
          (tileFlags collisionMap)
          (IntSet.toList . dirtyRegions $ collisionMap),
      dirtyRegions = IntSet.empty
    }

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
markSolidTile pos collisionMap =
  collisionMap
    { tileFlags = TileFlagsMap.setTileFlags blocksAllMovement pos (tileFlags collisionMap)
    }

markFlatWall :: Position -> Int -> CollisionMap -> CollisionMap
markFlatWall pos objType collisionMap =
  collisionMap {tileFlags = markFlatWall_ pos objType (tileFlags collisionMap)}

-- Flat Wall i.e. objType = 0
markFlatWall_ :: Position -> Int -> TileFlagsMap.TileFlagsMap -> TileFlagsMap.TileFlagsMap
markFlatWall_ pos 0 collisionMap =
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
markFlatWall_ pos 1 collisionMap =
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
markFlatWall_ pos 2 collisionMap =
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
markFlatWall_ pos 3 collisionMap =
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
markFlatWall_ _ _ c = c

allowsMovementBetween :: Position -> Position -> CollisionMap -> Bool
allowsMovementBetween origin destination collisionMap =
  let originTileFlags = TileFlagsMap.getTileFlags origin (tileFlags collisionMap)
   in let destinationTileFlags = TileFlagsMap.getTileFlags destination (tileFlags collisionMap)
       in (originTileFlags .&. directionFlag_ origin destination) == 0
            && (destinationTileFlags .&. directionFlag_ destination origin) == 0
            && not (isDiagonalAcrossSolidObject_ origin destination collisionMap) -- TODO - We should instead mark all neighbouring tiles of solid occupant for consistency  - keotl 2023-09-25

isDiagonalAcrossSolidObject_ :: Position -> Position -> CollisionMap -> Bool
isDiagonalAcrossSolidObject_ a b collisionMap =
  any
    ((== blocksAllMovement) . (`TileFlagsMap.getTileFlags` tileFlags collisionMap))
    (traversedDiagonalTiles_ a b)

traversedDiagonalTiles_ :: Position -> Position -> [Position]
traversedDiagonalTiles_ a b =
  let (dx, dy) = (x b - x a, y b - y a)
   in [a {x = x a + dx}, a {y = y a + dy}]

directionFlag_ :: Position -> Position -> Word8
directionFlag_ a b =
  case Direction.directionBetween (toXY a) (toXY b) of
    Direction.NorthWest -> blocksMovementNW
    Direction.North -> blocksMovementN
    Direction.NorthEast -> blocksMovementNE
    Direction.West -> blocksMovementW
    Direction.East -> blocksMovementE
    Direction.SouthWest -> blocksMovementSW
    Direction.South -> blocksMovementS
    Direction.SouthEast -> blocksMovementSE
    _ -> blocksAllMovement
