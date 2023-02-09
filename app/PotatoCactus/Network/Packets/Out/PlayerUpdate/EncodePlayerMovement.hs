module PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodePlayerMovement where

import Data.Binary.BitPut (BitPut, putBit, putNBits)
import qualified PotatoCactus.Game.Movement.Direction as Direction
import PotatoCactus.Game.Movement.MovementEntity (MovementEntity (PlayerWalkMovement_, StaticMovement_))
import PotatoCactus.Game.Movement.PlayerWalkMovement (PlayerWalkMovement (isTeleporting, runningDirection, walkingDirection))
import PotatoCactus.Game.Player (Player (movement))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (z), localY, localX)
import PotatoCactus.Network.Binary (toWord_)

encodePlayerMovement :: Player -> BitPut
encodePlayerMovement player =
  encode_ (movement player) True

encode_ :: MovementEntity -> Bool -> BitPut
encode_ (StaticMovement_ m) needsUpdate =
  do
    putBit True -- isTeleporting
    putNBits 2 (toWord_ 3)
    putNBits 2 (toWord_ 0) -- position.z
    putBit False -- region has changed
    putBit True -- needs update
    putNBits 7 $ toWord_ 53 * 8 -- local Y
    putNBits 7 $ toWord_ 52 * 8 -- local X
encode_ (PlayerWalkMovement_ m) needsUpdate =
  if isTeleporting m
    then do
      putBit True -- isTeleporting
      putNBits 2 $ toWord_ 3
      putNBits 2 $ toWord_ (z (getPosition m)) -- position.z
      putBit False -- region has changed
      putBit True -- needs update
      putNBits 7 $ toWord_ (localY (getPosition m)) -- local Y
      putNBits 7 $ toWord_ (localX (getPosition m)) -- local X
    else do
      case (walkingDirection m, runningDirection m) of
        (Direction.None, _) -> do
          if needsUpdate
            then do
              putBit True
              putNBits 2 $ toWord_ 0
            else do
              putBit False
        (walk, Direction.None) -> do
          putBit True
          putNBits 2 $ toWord_ 1
          putNBits 3 $ Direction.directionId walk
          putBit needsUpdate
        (walk, run) -> do
          putBit True
          putNBits 2 $ toWord_ 2
          putNBits 3 $ Direction.directionId walk
          putNBits 3 $ Direction.directionId run
          putBit needsUpdate
