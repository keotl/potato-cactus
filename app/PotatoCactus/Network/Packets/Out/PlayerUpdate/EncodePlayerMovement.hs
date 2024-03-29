module PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodePlayerMovement where

import Data.Binary.BitPut (BitPut, putBit, putNBits)
import qualified PotatoCactus.Game.Movement.Direction as Direction
import PotatoCactus.Game.Player (Player (movement, updateMask))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (z), localX, localY)
import PotatoCactus.Network.Binary (toWord_)
import PotatoCactus.Game.Movement.PlayerMovement (PlayerMovement(PlayerMovement, isTeleporting, hasChangedRegion, walkingDirection, runningDirection))

data MovementUpdateType = UpdateSelf | UpdateOther

encodePlayerMovement :: Player -> MovementUpdateType -> BitPut
encodePlayerMovement player updateType =
  encode_ (movement player) updateType (updateMask player > 0)

encode_ :: PlayerMovement -> MovementUpdateType -> Bool -> BitPut
encode_ m updateType needsUpdate =
  if isTeleporting m
    then do
      case updateType of
        UpdateSelf -> do
          putBit True -- isTeleporting
          putNBits 2 $ toWord_ 3
          putNBits 2 $ toWord_ (z (getPosition m)) -- position.z
          putBit $ not (hasChangedRegion m)
          putBit needsUpdate -- needs update
          putNBits 7 $ toWord_ (localY (getPosition m)) -- local Y
          putNBits 7 $ toWord_ (localX (getPosition m)) -- local X
          -- putBit needsUpdate
        UpdateOther -> do
          putBit False
          -- if needsUpdate
          --   then putNBits 2 $ toWord_ 3
          --   else putNBits 0 $ toWord_ 0
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
