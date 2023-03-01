module PotatoCactus.Network.Packets.In.ObjectActionPacket where

import Data.Binary (Get)
import Data.Binary.Get (getWord16be, getWord16le, runGet)
import Data.Bits (xor)
import Data.ByteString.Lazy (fromStrict)
import Data.Word (Word16)
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (ObjectClickMessage))
import PotatoCactus.Game.Message.ObjectClickPayload (ObjectClickPayload (ObjectClickPayload))
import PotatoCactus.Game.Movement.PositionXY (PositionXY (PositionXY))
import PotatoCactus.Network.Packets.Reader (InboundPacket (opcode, payload))

objectActionPacket :: String -> InboundPacket -> GameChannelMessage
objectActionPacket username packet =
  let (actionIndex, objectId, x, y) = runGet (selectReader_ (opcode packet)) (fromStrict . payload $ packet)
   in ObjectClickMessage
        username
        ( ObjectClickPayload
            (fromIntegral objectId)
            (PositionXY (fromIntegral x) (fromIntegral y))
            actionIndex
        )

selectReader_ :: Int -> Get (Int, Word16, Word16, Word16)
selectReader_ 132 = readPayloadFirstIndex_
selectReader_ 252 = readPayloadSecondIndex_
selectReader_ 70 = readPayloadThirdIndex_
selectReader_ _ = readPayloadFirstIndex_

readPayloadFirstIndex_ :: Get (Int, Word16, Word16, Word16)
readPayloadFirstIndex_ = do
  x <- getWord16le
  objectId <- getWord16be
  y <- getWord16be
  return (1, objectId, x `xor` 128, y `xor` 128)

readPayloadSecondIndex_ :: Get (Int, Word16, Word16, Word16)
readPayloadSecondIndex_ = do
  objectId <- getWord16le
  y <- getWord16le
  x <- getWord16be

  return (2, objectId `xor` 128, x `xor` 128, y)

readPayloadThirdIndex_ :: Get (Int, Word16, Word16, Word16)
readPayloadThirdIndex_ = do
  x <- getWord16le
  y <- getWord16be
  objectId <- getWord16le

  return (3, objectId `xor` 128, x, y)
