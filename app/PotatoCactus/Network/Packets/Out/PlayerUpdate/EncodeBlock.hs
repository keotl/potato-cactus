module PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeBlock where

import Data.Binary (Word16)
import Data.Binary.BitPut (BitPut, putNBits)
import Data.Bits ((.&.))
import Data.ByteString (ByteString, pack)
import PotatoCactus.Game.Player (Player (updateMask))
import PotatoCactus.Game.World
import PotatoCactus.Network.Binary (toWord_)

addBlockIfRequired :: Word16 -> (Player -> World -> ByteString) -> (Word16 -> (Player -> World -> ByteString))
addBlockIfRequired updateFlag createBlock mask =
  if (mask .&. updateFlag) > 0
    then createBlock
    else empty_

empty_ :: Player -> World -> ByteString
empty_ _ _ = pack []
