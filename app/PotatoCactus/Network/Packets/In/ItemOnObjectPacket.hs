module PotatoCactus.Network.Packets.In.ItemOnObjectPacket (itemOnObjectPacket) where

import Data.Binary.Get (Get, getInt16le, getWord16be, getWord16le, runGet)
import Data.Bits (xor)
import Data.ByteString.Lazy (fromStrict)
import Data.Int (Int16)
import Data.Word (Word16)
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (ItemOnObjectMessage))
import PotatoCactus.Game.Message.ItemOnObjectPayload (ItemOnObjectPayload (ItemOnObjectPayload))
import PotatoCactus.Game.Movement.PositionXY (PositionXY (PositionXY))
import PotatoCactus.Game.Player (PlayerIndex)
import PotatoCactus.Network.Packets.Reader (InboundPacket (payload))

itemOnObjectPacket :: PlayerIndex -> InboundPacket -> GameChannelMessage
itemOnObjectPacket playerId packet =
  let ( itemInterfaceId,
        objectId,
        objectY,
        itemIndexId,
        objectX,
        itemId
        ) = runGet readPayload_ (fromStrict . payload $ packet)
   in ItemOnObjectMessage
        playerId
        (fromIntegral itemInterfaceId)
        (fromIntegral objectId)
        (PositionXY (fromIntegral objectX) (fromIntegral objectY))
        (fromIntegral itemIndexId)
        (fromIntegral itemId)

readPayload_ :: Get (Word16, Word16, Int16, Word16, Int16, Word16)
readPayload_ = do
  itemInterfaceId <- getWord16be
  objectId <- getWord16le
  objectY <- getInt16le
  itemIndexId <- getWord16le
  objectX <- getInt16le
  itemId <- getWord16be
  return (itemInterfaceId, objectId, objectY `xor` 128, itemIndexId, objectX `xor` 128, itemId)
