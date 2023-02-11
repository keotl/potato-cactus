module PotatoCactus.Network.Packets.Out.ResetCameraPacket where

import Data.Binary.BitPut (putNBits)
import Data.ByteString (ByteString)
import PotatoCactus.Network.Binary (toWord_)
import PotatoCactus.Network.Packets.Packet (fixedPacket)

resetCameraPacket :: ByteString
resetCameraPacket =
  fixedPacket 107 (do putNBits 0 $ toWord_ 0)
