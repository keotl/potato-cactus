module PotatoCactus.Network.InboundPacketMapper where

import PotatoCactus.Boot.GameChannel (GameChannelMessage (UnregisterClientMessage))
import PotatoCactus.Network.Packets.Opcodes (socketClosedOpcode)
import PotatoCactus.Network.Packets.Reader (InboundPacket (opcode))

mapPacket :: String -> InboundPacket -> Maybe GameChannelMessage
mapPacket clientIdentifier packet =
  case opcode packet of
    254 -> Just $ UnregisterClientMessage clientIdentifier
    _ -> Nothing
