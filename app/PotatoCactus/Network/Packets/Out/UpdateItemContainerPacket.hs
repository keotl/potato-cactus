module PotatoCactus.Network.Packets.Out.UpdateItemContainerPacket where

import Data.Binary.Put (Put, putWord16be, putWord16le, putWord8)
import Data.Bits (Bits (xor))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (empty, repeat, toStrict)
import PotatoCactus.Game.ItemContainer (ItemContainer (content, updated), ItemStack (itemId, quantity))
import qualified PotatoCactus.Game.ItemContainer as IC (ItemContainer (capacity, widgetId), ItemStack (Empty))
import PotatoCactus.Network.Binary (toShortLE_, toShort_, toWord_)
import PotatoCactus.Network.Packets.Packet (varShortPacket2)
import Prelude hiding (id)

updateItemContainerPacket :: IC.ItemContainer -> ByteString
updateItemContainerPacket container
  | updated container =
    varShortPacket2
      53
      ( do
          putWord16be $ fromIntegral (IC.widgetId container)
          putWord16be $ fromIntegral (IC.capacity container)
          mapM_ putItemStack_ (content container)
      )
  | otherwise = toStrict empty

putItemStack_ :: IC.ItemStack -> Put
putItemStack_ IC.Empty = do
  putWord8 $ toWord_ 0
  putWord16le $ fromIntegral (0 + 128)
putItemStack_ item = do
  putWord8 $ toWord_ (quantity item)
  putWord16le $ fromIntegral (itemId item `xor` 128) + 1

-- TODO - implement with large quantities items, int32 - keotl 2023-02-05
