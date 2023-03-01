module PotatoCactus.Network.Packets.In.EquipItemPacket (equipItemPacket) where

import Data.Binary (Get, Word16)
import Data.Binary.Get (getWord16be, runGet)
import Data.Bits (xor)
import Data.ByteString.Lazy (fromStrict)
import PotatoCactus.Game.Message.EquipItemMessagePayload (EquipItemMessagePayload (EquipItemMessagePayload, itemId))
import PotatoCactus.Network.Packets.Reader (InboundPacket (InboundPacket, payload))
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (EquipItemMessage))

equipItemPacket :: String -> InboundPacket -> GameChannelMessage
equipItemPacket username packet =
  let (itemId, index, widgetId) = runGet readPayload_ (fromStrict . payload $ packet)
   in EquipItemMessage
        username
        ( EquipItemMessagePayload
            (fromIntegral itemId)
            (fromIntegral index)
            (fromIntegral widgetId)
        )

readPayload_ :: Get (Word16, Word16, Word16)
readPayload_ = do
  itemId <- getWord16be
  index <- getWord16be
  widgetId <- getWord16be
  return (itemId, index `xor` 128, widgetId `xor` 128)
