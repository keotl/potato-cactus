module PotatoCactus.Network.Packets.Out.LoadMapRegionPacket where

import Data.Binary.BitPut (putNBits)
import Data.ByteString (ByteString)
import PotatoCactus.Game.Position (Position, chunkX, chunkY)
import PotatoCactus.Network.Packets.Packet (fixedPacket)
import PotatoCactus.Network.Binary (toShort_)

loadMapRegionPacket :: Position -> ByteString
loadMapRegionPacket playerPos =
  fixedPacket
    73
    ( do
        putNBits 16 $ toShort_ $ chunkX playerPos + 6 - 128
        putNBits 16 $ toShort_ $ chunkY playerPos + 6
    )
