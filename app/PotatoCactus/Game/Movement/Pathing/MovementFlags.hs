module PotatoCactus.Game.Movement.Pathing.MovementFlags where

import Data.Binary (Word8)

allowsFullMovement :: Word8
allowsFullMovement = 0

blocksMovementNW :: Word8
blocksMovementNW = 1

blocksMovementN :: Word8
blocksMovementN = 2

blocksMovementNE :: Word8
blocksMovementNE = 3

blocksMovementE :: Word8
blocksMovementE = 4

blocksMovementSE :: Word8
blocksMovementSE = 5

blocksMovementS :: Word8
blocksMovementS = 6

blocksMovementSW :: Word8
blocksMovementSW = 7

blocksMovementW :: Word8
blocksMovementW = 8
