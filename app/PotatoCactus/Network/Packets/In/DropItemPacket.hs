module PotatoCactus.Network.Packets.In.DropItemPacket (dropItemPacket) where

import Data.Binary (Get)
import Data.Binary.Get (getWord16be, getWord16le, runGet)
import Data.Bits (Bits (xor))
import Data.ByteString.Lazy (fromStrict)
import Data.Word (Word16)
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (DropItemMessage))
import PotatoCactus.Game.Player (PlayerIndex)
import PotatoCactus.Network.Packets.Reader (InboundPacket (payload))

dropItemPacket :: PlayerIndex -> InboundPacket -> GameChannelMessage
dropItemPacket playerIndex packet =
  let (itemId, widgetId, index) = runGet readPayload_ (fromStrict . payload $ packet)
   in DropItemMessage
        playerIndex
        (fromIntegral widgetId)
        (fromIntegral itemId)
        (fromIntegral index)

readPayload_ :: Get (Word16, Word16, Word16)
readPayload_ = do
  itemId <- getWord16be
  widgetId <- getWord16be
  index <- getWord16be
  return (itemId `xor` 128, widgetId, index `xor` 128)
