module PotatoCactus.Network.Packets.Out.RemoveObjectPacket where

import Data.Binary.BitPut (putNBits)
import Data.Binary.Put (putInt8, putWord8)
import Data.Bits (Bits (shiftL, (.&.)))
import Data.ByteString (ByteString)
import PotatoCactus.Game.Definitions.GameObjectDefinitions (GameObjectDefinition (objectType), objectDefinition)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (facingDirection, id))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (x, y), localToRefX, localToRefY)
import PotatoCactus.Network.Binary (toWord_)
import PotatoCactus.Network.Packets.Packet (fixedPacket2)
import Prelude hiding (id)

removeObjectPacket :: Position -> GameObject -> ByteString
removeObjectPacket refPos object =
  fixedPacket2
    101
    ( do
        case objectDefinition (id object) of
          Just def -> do
            putInt8 . fromIntegral $ (- (objectType def * 4 + facingDirection object))
            let offset =
                  ( ((x refPos - (x . getPosition $ object)) `shiftL` 4)
                      + ((y refPos - (y . getPosition $ object)) .&. 7)
                  )
             in do
                  putWord8 $ fromIntegral offset
          Nothing -> do
            -- dummy fallback, remove interactible on tile
            putInt8 (- (10 * 4 + 0)) -- interactible
            putWord8 0
    )
