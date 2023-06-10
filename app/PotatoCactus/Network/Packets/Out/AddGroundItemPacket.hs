module PotatoCactus.Network.Packets.Out.AddGroundItemPacket where

import Data.Binary.Put (putWord16be, putWord16le, putWord8)
import Data.Bits (Bits (xor, (.&.)), shiftL)
import Data.ByteString (ByteString)
import PotatoCactus.Client.GroundItemsUpdate.GroundItemsUpdateDiff (GroundItemClientView (quantity), itemId)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (x, y))
import PotatoCactus.Network.Packets.Packet (fixedPacket2)

addGroundItemPacket :: Position -> GroundItemClientView -> ByteString
addGroundItemPacket refPos item =
  fixedPacket2
    44
    ( do
        let offset =
              ( ((x refPos - (x . getPosition $ item)) `shiftL` 4)
                  + ((y refPos - (y . getPosition $ item)) .&. 7)
              )
         in do
              putWord16le . fromIntegral $ (itemId item `xor` 128)
              putWord16be . fromIntegral . quantity $ item
              putWord8 . fromIntegral $ offset - 128 -- TODO - hardcoded to 0 in Luna. Test whether it is the same format as game object  - keotl 2023-06-10
    )
