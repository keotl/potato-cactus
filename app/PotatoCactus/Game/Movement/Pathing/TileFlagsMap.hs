module PotatoCactus.Game.Movement.Pathing.TileFlagsMap (TileFlagsMap, create, getTileFlags, setTileFlags) where

import Data.Binary (Word64, Word8)
import Data.Bits (Bits (complement, shiftR, (.&.), (.|.)), shiftL)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import PotatoCactus.Game.Position (Position (x, y, z))
import PotatoCactus.Utils.Iterable (alterAtIndex, replaceAtIndex)

type TileFlags = Word8

type TileContainer = Word64

type RegionMap = [TileContainer]

data TileFlagsMap = TileFlagsMap
  { regions :: IntMap.IntMap (RegionMap)
  }
  deriving (Show, Eq)

create :: TileFlagsMap
create = TileFlagsMap IntMap.empty

getTileFlags :: Position -> TileFlagsMap -> TileFlags
getTileFlags pos collisionMap =
  case regions collisionMap IntMap.!? regionKey pos of
    Nothing -> 0
    Just regionMap -> getTileInContainer pos (regionMap !! regionMapOffset (x pos) (y pos))

setTileFlags :: TileFlags -> Position -> TileFlagsMap -> TileFlagsMap
setTileFlags flags pos collisionMap =
  collisionMap
    { regions =
        IntMap.alter
          ( Just
              . alterAtIndex
                (regionMapOffset (x pos) (y pos))
                (setTileInContainer flags pos)
              . fromMaybe emptyRegionMap
          )
          (regionKey pos)
          (regions collisionMap)
    }

regionKey :: Position -> Int
regionKey pos =
  (x pos `div` 64) + ((y pos `div` 64) * 1000) + (z pos * 1000000)

setTileInContainer :: TileFlags -> Position -> TileContainer -> TileContainer
setTileInContainer updated pos old =
  let offset = tileContainerOffset (x pos) (y pos)
   in let withZeroed = old .&. complement (tileContainerMask offset)
       in old .|. (toWord64 updated `shiftL` (offset * 8))

getTileInContainer :: Position -> TileContainer -> TileFlags
getTileInContainer pos container =
  let offset = tileContainerOffset (x pos) (y pos)
   in toWord8 ((container .&. tileContainerMask offset) `shiftR` (offset * 8))

emptyRegionMap :: RegionMap
emptyRegionMap = [0 | i <- [1 .. 512]]

regionMapOffset :: Int -> Int -> Int
regionMapOffset posX posY =
  ((posX `mod` 64) * 64 + (posY `mod` 64)) `div` 8

tileContainerOffset :: Int -> Int -> Int
tileContainerOffset posX posY =
  ((posX `mod` 64) * 64 + (posY `mod` 64)) `mod` 8

tileContainerMask :: Int -> Word64
tileContainerMask offset =
  255 `shiftL` (8 * offset)

toWord64 :: Word8 -> Word64
toWord64 = fromIntegral

toWord8 :: Word64 -> Word8
toWord8 = fromIntegral
