module PotatoCactus.Network.InboundPacketMapper where

import PotatoCactus.Boot.GameChannel (GameChannelMessage (PlayerWalkMessage, UnregisterClientMessage))
import PotatoCactus.Network.Packets.In.PlayerWalkPacket (playerWalkMessage)
import PotatoCactus.Network.Packets.Opcodes (socketClosedOpcode)
import PotatoCactus.Network.Packets.Reader (InboundPacket (opcode))

mapPacket :: String -> InboundPacket -> Maybe GameChannelMessage
mapPacket clientIdentifier packet =
  case opcode packet of
    254 -> Just $ UnregisterClientMessage clientIdentifier
    164 -> Just $ playerWalkMessage clientIdentifier packet
    _ -> Nothing
