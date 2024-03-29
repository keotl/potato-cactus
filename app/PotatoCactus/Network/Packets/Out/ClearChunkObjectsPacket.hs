module PotatoCactus.Network.Packets.Out.ClearChunkObjectsPacket where

import Data.Binary.Put (putInt8, putWord8)
import Data.ByteString (ByteString, concat)
import PotatoCactus.Game.Movement.PlayerMovement (lastRegionUpdate_)
import PotatoCactus.Game.Player (Player (Player, movement))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position, z), chunkX, chunkY, localToRefX, localToRefY)
import PotatoCactus.Network.Packets.Packet (fixedPacket2)

clearChunkObjectsPacket :: Position -> Player -> ByteString
clearChunkObjectsPacket chunkPos p =
  fixedPacket2
    64
    ( do
        let region = lastRegionUpdate_ (movement p)
         in do
              putInt8 $ fromIntegral (- localToRefX region chunkPos)
              putInt8 $ fromIntegral $ 128 - localToRefY region chunkPos
    )

clearChunksAroundPlayer :: Player -> ByteString
clearChunksAroundPlayer player =
  let chunkPosX = chunkX (getPosition player)
   in let chunkPosY = chunkY (getPosition player)
       in Data.ByteString.concat
            [ clearChunkObjectsPacket
                (Position ((chunkPosX + x + 6) * 8) ((chunkPosY + y + 6) * 8) (z . getPosition $ player))
                player
              | x <- [-2 .. 1],
                y <- [-2 .. 1]
            ]
