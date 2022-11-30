module PotatoCactus.Network.InboundPacketMapper where

import PotatoCactus.Boot.GameChannel (GameChannelMessage (UnregisterClientMessage))
import PotatoCactus.Network.Packets.Reader (InboundPacket (opcode))
import PotatoCactus.Network.Packets.Opcodes (socketClosedOpcode)

mapPacket :: String -> InboundPacket -> Maybe GameChannelMessage
mapPacket clientIdentifier packet =
  case opcode packet of
    socketClosedOpcode -> Just $ UnregisterClientMessage clientIdentifier
    _ -> Nothing
