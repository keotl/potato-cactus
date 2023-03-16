module PotatoCactus.Network.Packets.Out.AddObjectPacket where

import Data.Binary.BitPut (putNBits)
import Data.Binary.Put (putWord16le, putWord8)
import Data.Bits (Bits (shiftL, (.&.)), shiftR)
import Data.ByteString (ByteString)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (facingDirection, id, objectType))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (x, y), localToRefX, localToRefY)
import PotatoCactus.Network.Binary (toShortLE_, toShort_, toWord_)
import PotatoCactus.Network.Packets.Packet (fixedPacket2)
import Prelude hiding (id)

addObjectPacket :: Position -> GameObject -> ByteString
addObjectPacket refPos object =
  fixedPacket2
    151
    ( do
        let offset =
              ( ((x refPos - (x . getPosition $ object)) `shiftL` 4)
                  + ((y refPos - (y . getPosition $ object)) .&. 7)
              )
         in do
              putWord8 . fromIntegral $ offset - 128
              putWord16le . fromIntegral . id $ object
              putWord8 . fromIntegral $ (128 - (objectType object * 4 + facingDirection object))
    )
