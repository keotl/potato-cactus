module PotatoCactus.Network.Packets.Out.ChatboxMessagePacket where

import Data.Binary.BitPut (putByteString, putNBits, runBitPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Network.Binary (encodeStr, toWord_)
import PotatoCactus.Network.Packets.Packet (varPacket)

chatboxMessagePacket :: String -> ByteString
chatboxMessagePacket message =
  varPacket
    253
    ( do
        putByteString $ encodeStr message
    )
