module PotatoCactus.Network.Packets.Out.InitializePlayerPacket where

import Data.Binary.BitPut (putNBits, runBitPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Network.Binary (toShortLE_, toWord_)

initializePlayerPacket :: Bool -> Int -> ByteString
initializePlayerPacket isMember serverIndex =
  toStrict $
    runBitPut
      ( do
          putNBits 8 $ toWord_ 249
          putNBits 8 $ toWord_ (128 + (if isMember then 1 else 0))
          putNBits 16 $ toShortLE_ (128 + serverIndex)
      )
