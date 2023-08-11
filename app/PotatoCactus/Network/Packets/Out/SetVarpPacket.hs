module PotatoCactus.Network.Packets.Out.SetVarpPacket where

import Data.Binary.Put (putWord16le, putWord32be)
import Data.ByteString (ByteString)
import PotatoCactus.Network.Binary (toIntME_)
import PotatoCactus.Network.Packets.Packet (fixedPacket2)

setVarpPacket :: Int -> Int -> ByteString
setVarpPacket varpId value =
  fixedPacket2
    87
    ( do
        putWord16le . fromIntegral $ varpId
        putWord32be . toIntME_ $ value
    )
