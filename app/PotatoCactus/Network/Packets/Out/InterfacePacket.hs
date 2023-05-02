module PotatoCactus.Network.Packets.Out.InterfacePacket where

import Data.Binary (Word16)
import Data.Binary.Put (putWord16be)
import Data.ByteString (ByteString)
import PotatoCactus.Network.Packets.Packet (fixedPacket2)

interfacePacket :: Word16 -> ByteString
interfacePacket interfaceId =
  fixedPacket2
    97
    ( do
        putWord16be interfaceId
    )
