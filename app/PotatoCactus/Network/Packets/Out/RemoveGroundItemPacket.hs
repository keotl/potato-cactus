module PotatoCactus.Network.Packets.Out.RemoveGroundItemPacket where

import Data.Binary.Put (putWord16be, putWord8)
import Data.Bits (shiftL, (.&.))
import Data.ByteString (ByteString)
import PotatoCactus.Client.GroundItemsUpdate.GroundItemsUpdateDiff (GroundItemClientView (itemId))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (x, y), localToRefX, localToRefY)
import PotatoCactus.Network.Packets.Packet (fixedPacket2)

removeGroundItemPacket :: Position -> GroundItemClientView -> ByteString
removeGroundItemPacket refPos item =
  fixedPacket2
    156
    ( let offset =
            ( ((x refPos - (x . getPosition $ item)) `shiftL` 4)
                + ((y refPos - (y . getPosition $ item)) .&. 7)
            )
       in do
            putWord8 $ fromIntegral (128 - offset)
            putWord16be . fromIntegral . itemId $ item
    )
