module PotatoCactus.Network.Packets.Out.ChatboxMessagePacket where

import Data.Binary.BitPut (putByteString, putNBits, runBitPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Network.Binary (encodeStr, toWord_)

chatboxMessagePacket :: String -> ByteString
chatboxMessagePacket message =
  toStrict $
    runBitPut
      ( do
          putNBits 8 $ toWord_ 253
          putByteString $ encodeStr message
      )
