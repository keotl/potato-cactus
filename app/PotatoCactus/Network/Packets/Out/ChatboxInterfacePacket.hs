module PotatoCactus.Network.Packets.Out.ChatboxInterfacePacket where

import Data.Binary (Word16)
import Data.Binary.Put (putWord16le)
import Data.ByteString (ByteString)
import PotatoCactus.Network.Packets.Packet (fixedPacket2)

chatboxInterfacePacket :: Word16 -> ByteString
chatboxInterfacePacket interfaceId =
  fixedPacket2
    164
    ( do
        putWord16le interfaceId
    )
