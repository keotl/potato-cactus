module PotatoCactus.Network.Packets.Out.TabInterfacePacket where

import Data.Binary (Word16, Word8)
import Data.Binary.BitPut (putNBits, runBitPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Network.Binary (toWord_, toShort_)

tabInterfacePacket :: Int -> Int -> ByteString
tabInterfacePacket tabIndex interfaceId =
  toStrict $
    runBitPut
      ( do
          putNBits 8 $ toWord_ 71
          putNBits 16 $ toShort_ interfaceId
          putNBits 8 $ toWord_ tabIndex
      )
