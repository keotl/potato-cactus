module PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeBlock where

import Data.Binary (Word16)
import Data.Binary.BitPut (BitPut, putNBits)
import Data.Bits ((.&.))
import PotatoCactus.Game.Player (Player (updateMask))
import PotatoCactus.Network.Binary (toWord_)

encodeBlock :: Word16 -> (Player -> BitPut) -> (Player -> BitPut)
encodeBlock flag assemble p =
  if updateMask p .&. flag > 0
    then assemble p
    else putNBits 0 $ toWord_ 0
