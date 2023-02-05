module PotatoCactus.Network.Packets.Out.TabInterfacePacket where

import Data.Binary (Word16, Word8)
import Data.Binary.BitPut (putNBits, runBitPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Network.Binary (toShort_, toWord_)
import PotatoCactus.Network.Packets.Packet (fixedPacket)

tabInterfacePacket :: Int -> Int -> ByteString
tabInterfacePacket tabIndex interfaceId =
  fixedPacket
    71
    ( do
        putNBits 16 $ toShort_ interfaceId
        putNBits 8 $ toWord_ (128 + tabIndex)
    )
