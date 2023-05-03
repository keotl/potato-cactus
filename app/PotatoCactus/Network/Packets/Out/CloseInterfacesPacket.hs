module PotatoCactus.Network.Packets.Out.CloseInterfacesPacket where

import Data.Binary.Put (putByteString)
import Data.ByteString (ByteString, empty)
import PotatoCactus.Network.Packets.Packet (fixedPacket2)

closeInterfacesPacket :: ByteString
closeInterfacesPacket =
  fixedPacket2 219 (putByteString empty)
