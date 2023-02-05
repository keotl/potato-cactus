module PotatoCactus.Network.Packets.Out.UpdateRunEnergyPacket where

import Data.Binary.BitPut (putNBits, runBitPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Network.Binary (toWord_)

updateRunEnergyPacket :: Int -> ByteString
updateRunEnergyPacket level =
  toStrict $
    runBitPut $ do
      putNBits 8 $ toWord_ level
