module PotatoCactus.Network.Packets.In.NpcAttackPacket (npcAttackPacket) where

import Data.Binary.Get (getWord16be, runGet)
import Data.Bits (xor)
import Data.ByteString.Lazy (fromStrict)
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (NpcAttackMessage))
import PotatoCactus.Game.Player (PlayerIndex)
import PotatoCactus.Network.Packets.Reader (InboundPacket (InboundPacket, payload))

npcAttackPacket :: PlayerIndex -> InboundPacket -> GameChannelMessage
npcAttackPacket playerId packet =
  let npcIndex = runGet getWord16be (fromStrict . payload $ packet)
   in NpcAttackMessage playerId (fromIntegral (npcIndex `xor` 128))
