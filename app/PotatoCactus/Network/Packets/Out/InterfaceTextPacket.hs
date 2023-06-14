module PotatoCactus.Network.Packets.Out.InterfaceTextPacket where

import Data.Binary (Word16)
import Data.Binary.Put (putByteString, putWord16be)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import PotatoCactus.Network.Binary (encodeStr)
import PotatoCactus.Network.Packets.Packet (varShortPacket2)

interfaceTextPacket :: Word16 -> String -> ByteString
interfaceTextPacket interfaceId text =
  varShortPacket2
    126
    ( do
        putByteString . encodeStr $ text
        putWord16be (interfaceId `xor` 128)
    )
