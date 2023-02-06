module PotatoCactus.Network.Packets.Out.UpdateRunEnergyPacket where

import Data.Binary.BitPut (putNBits, runBitPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Network.Binary (toWord_)
import PotatoCactus.Network.Packets.Packet (fixedPacket)

updateRunEnergyPacket :: Int -> ByteString
updateRunEnergyPacket level =
  fixedPacket
    110
    ( do
        putNBits 8 $ toWord_ level
    )
