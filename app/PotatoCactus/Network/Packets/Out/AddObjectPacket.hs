module PotatoCactus.Network.Packets.Out.AddObjectPacket where

import Data.Binary.BitPut (putNBits)
import Data.Bits (Bits (shiftL, (.&.)), shiftR)
import Data.ByteString (ByteString)
import Debug.Trace (trace)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (id))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (x, y), localToRefX, localToRefY)
import PotatoCactus.Network.Binary (toShortLE_, toShort_, toWord_)
import PotatoCactus.Network.Packets.Packet (fixedPacket)
import Prelude hiding (id)

addObjectPacket :: Position -> GameObject -> ByteString
addObjectPacket refPos object =
  fixedPacket
    151
    ( do
        -- putNBits 4 . toWord_ $ localToRefX refPos (getPosition object)
        -- putNBits 4 . toWord_ $ localToRefY refPos (getPosition object)

        let offset =
              ( ((x refPos - (x . getPosition $ object)) `shiftL` 4)
                  + ((y refPos - (y . getPosition $ object)) .&. 7)
              )
         in do
              putNBits 8 $ toWord_ (offset - 128)
              -- putNBits 4 . toWord_ $ 0
              -- putNBits 4 . toWord_ $ 0
              putNBits 16 . toShortLE_ . id $ object
              putNBits 8 $ toWord_ (128 - (10 * 4 + 0))
              -- putNBits 6 . toWord_ $ 0 -- type
              -- putNBits 2 . toWord_ $ 0 -- orientation
    )
