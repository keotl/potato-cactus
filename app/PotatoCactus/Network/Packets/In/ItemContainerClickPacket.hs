module PotatoCactus.Network.Packets.In.ItemContainerClickPacket where

import Data.Binary (Get)
import Data.Binary.Get (getInt16le, getWord16be, getWord16le, runGet)
import Data.Bits (Bits (xor))
import Data.ByteString.Lazy (fromStrict)
import Data.Word (Word16)
import Debug.Trace (trace)
import PotatoCactus.Network.Packets.Reader (InboundPacket (InboundPacket, opcode, payload))
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (UnequipItemMessage))

itemContainerClickPacket :: String -> InboundPacket -> Maybe GameChannelMessage
itemContainerClickPacket username packet =
  let (actionIndex, itemId, index, widgetId) = runGet (selectReader_ (opcode packet)) (fromStrict . payload $ packet)
   in case (actionIndex, widgetId) of
        (1, 1688) -> Just $ UnequipItemMessage username (fromIntegral index)
        _ -> Nothing

selectReader_ :: Int -> Get (Int, Word16, Word16, Word16)
selectReader_ 145 = readPayloadFirstIndex_
selectReader_ _ = readPayloadFirstIndex_

readPayloadFirstIndex_ :: Get (Int, Word16, Word16, Word16)
readPayloadFirstIndex_ = do
  widgetId <- getWord16be
  index <- getWord16be
  itemId <- getWord16be
  return (1, itemId `xor` 128, index `xor` 128, widgetId `xor` 128)

readPayloadSecondIndex_ :: Get (Int, Word16, Word16, Word16)
readPayloadSecondIndex_ = do
  widgetId <- getInt16le
  itemId <- getInt16le
  index <- getInt16le
  return (2, fromIntegral itemId `xor` 128, fromIntegral index, fromIntegral widgetId `xor` 128)

readPayloadThirdIndex_ :: Get (Int, Word16, Word16, Word16)
readPayloadThirdIndex_ = do
  widgetId <- getWord16le
  itemId <- getWord16be
  index <- getWord16be
  return (3, itemId `xor` 128, index `xor` 128, widgetId)

readPayloadFourthIndex_ :: Get (Int, Word16, Word16, Word16)
readPayloadFourthIndex_ = do
  index <- getWord16be
  widgetId <- getWord16be
  itemId <- getWord16be

  return (4, itemId `xor` 128, index `xor` 128, widgetId)

readPayloadFifthIndex_ :: Get (Int, Word16, Word16, Word16)
readPayloadFifthIndex_ = do
  index <- getWord16le
  widgetId <- getWord16be
  itemId <- getWord16le

  return (5, itemId, index, widgetId `xor` 128)
