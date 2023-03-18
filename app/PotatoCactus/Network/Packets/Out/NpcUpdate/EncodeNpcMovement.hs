module PotatoCactus.Network.Packets.Out.NpcUpdate.EncodeNpcMovement where

import Data.Binary.BitPut (BitPut, putBit, putNBits)
import PotatoCactus.Game.Entity.Npc.Npc (Npc (movement, updateMask))
import PotatoCactus.Game.Entity.Npc.NpcMovement (walkingDirection)
import PotatoCactus.Game.Movement.Direction (Direction (None), directionId)
import PotatoCactus.Network.Binary (toWord_)

encodeNpcMovement :: Npc -> BitPut
encodeNpcMovement npc = do
  let updateRequired = updateMask npc > 0
   in case walkingDirection . movement $ npc of
        None -> do
          if updateRequired
            then do
              putBit True
              putNBits 2 $ toWord_ 0
            else do
              putBit False
        dir -> do
          putBit True
          putNBits 2 $ toWord_ 1
          putNBits 3 $ directionId dir
          putBit updateRequired
