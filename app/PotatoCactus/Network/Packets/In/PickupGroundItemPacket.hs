module PotatoCactus.Network.Packets.In.PickupGroundItemPacket (pickupGroundItemPacket) where

import Data.Binary (Word16)
import Data.Binary.Get (Get, getWord16be, getWord16le, runGet)
import Data.ByteString.Lazy (fromStrict)
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (PickupGroundItemMessage))
import PotatoCactus.Game.Movement.PositionXY (PositionXY (PositionXY))
import PotatoCactus.Game.Player (PlayerIndex)
import PotatoCactus.Network.Packets.Reader (InboundPacket (payload))

pickupGroundItemPacket :: PlayerIndex -> InboundPacket -> GameChannelMessage
pickupGroundItemPacket playerIndex packet =
  let (y, itemId, x) = runGet readPayload_ (fromStrict . payload $ packet)
   in PickupGroundItemMessage
        playerIndex
        (fromIntegral itemId)
        (PositionXY (fromIntegral x) (fromIntegral y))

readPayload_ :: Get (Word16, Word16, Word16)
readPayload_ = do
  y <- getWord16le
  itemId <- getWord16be
  x <- getWord16le
  return (y, itemId, x)
