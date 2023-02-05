module PotatoCactus.Network.Packets.Out.UpdateItemContainerPacket where

import Data.Binary.BitPut (BitPut, putNBits, runBitPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (repeat, toStrict)
import qualified PotatoCactus.Game.ItemContainer as IC (ItemContainer (capacity, widgetId), ItemStack (Empty))
import PotatoCactus.Network.Binary (toShortLE_, toShort_, toWord_)

updateItemContainerPacket :: IC.ItemContainer -> ByteString
updateItemContainerPacket container =
  toStrict $
    runBitPut
      ( do
          putNBits 8 $ toWord_ 53
          putNBits 16 $ toShort_ (IC.widgetId container)
          putNBits 16 $ toShort_ (IC.capacity container)
          mapM_ putItemStack_ (mockItems_ container)
      )

putItemStack_ :: IC.ItemStack -> BitPut
putItemStack_ IC.Empty = do
  putNBits 8 $ toWord_ 0
  putNBits 16 $ toShortLE_ (0 + 128)

putItemStack_ item = do
  -- TODO - implement with items - keotl 2023-02-05
  putNBits 8 $ toWord_ 0
  putNBits 16 $ toShortLE_ (0 + 128)

mockItems_ :: IC.ItemContainer -> [IC.ItemStack]
mockItems_ container =
  replicate (IC.capacity container) IC.Empty
