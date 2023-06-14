module PotatoCactus.Network.Packets.Out.InterfaceAnimationPacket where

import Data.Binary (Word16)
import Data.Binary.Put (putWord16be)
import Data.ByteString (ByteString)
import PotatoCactus.Network.Packets.Packet (fixedPacket2)

interfaceAnimationPacket :: Word16 -> Word16 -> ByteString
interfaceAnimationPacket interfaceId animationId =
  fixedPacket2
    200
    ( do
        putWord16be interfaceId
        putWord16be animationId
    )
