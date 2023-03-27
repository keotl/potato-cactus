module PotatoCactus.Network.Packets.In.NpcActionPacket (npcActionPacket) where

import Data.Binary (Get)
import Data.Binary.Get (getWord16be, getWord16le, runGet)
import Data.Bits (xor)
import Data.ByteString.Lazy (fromStrict)
import Data.Word (Word16)
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (NpcClickMessage))
import PotatoCactus.Game.Player (PlayerIndex)
import PotatoCactus.Network.Packets.Reader (InboundPacket (opcode, payload))

npcActionPacket :: PlayerIndex -> InboundPacket -> Maybe GameChannelMessage
npcActionPacket playerId packet =
  let (actionIndex, npcId) = runGet (selectReader_ (opcode packet)) (fromStrict . payload $ packet)
   in Just $
        NpcClickMessage
          playerId
          (fromIntegral npcId)
          actionIndex

selectReader_ :: Int -> Get (Int, Word16)
selectReader_ 155 = readPayloadFirstIndex_
selectReader_ 17 = readPayloadSecondIndex_
selectReader_ 21 = readPayloadThirdIndex_
selectReader_ 18 = readPayloadFourthIndex_
selectReader_ _ = readPayloadFirstIndex_

readPayloadFirstIndex_ :: Get (Int, Word16)
readPayloadFirstIndex_ = do
  npcId <- getWord16le
  return (1, npcId)

readPayloadSecondIndex_ :: Get (Int, Word16)
readPayloadSecondIndex_ = do
  npcId <- getWord16le
  return (2, npcId `xor` 128)

readPayloadThirdIndex_ :: Get (Int, Word16)
readPayloadThirdIndex_ = do
  npcId <- getWord16be
  return (3, npcId)

readPayloadFourthIndex_ :: Get (Int, Word16)
readPayloadFourthIndex_ = do
  npcId <- getWord16le
  return (4, npcId)
