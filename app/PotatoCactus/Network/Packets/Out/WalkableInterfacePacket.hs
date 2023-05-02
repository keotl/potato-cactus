module PotatoCactus.Network.Packets.Out.WalkableInterfacePacket where

import Data.Binary (Word16)
import Data.Binary.Put (putWord16le)
import Data.ByteString (ByteString)
import PotatoCactus.Network.Packets.Packet (fixedPacket2)

walkableInterfacePacket :: Word16 -> ByteString
walkableInterfacePacket interfaceId =
  fixedPacket2
    208
    ( do
        putWord16le interfaceId
    )
