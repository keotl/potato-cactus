module PotatoCactus.Network.InboundPacketMapper where

import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (UnregisterClientMessage))
import PotatoCactus.Game.Player (PlayerIndex)
import PotatoCactus.Network.Packets.In.ButtonClickPacket (buttonClickMessage)
import PotatoCactus.Network.Packets.In.ChatMessagePacket (playerChatMessage)
import PotatoCactus.Network.Packets.In.ContinueDialoguePacket (continueDialoguePacket)
import PotatoCactus.Network.Packets.In.EquipItemPacket (equipItemPacket)
import PotatoCactus.Network.Packets.In.ItemContainerClickPacket (itemContainerClickPacket)
import PotatoCactus.Network.Packets.In.NpcActionPacket (npcActionPacket)
import PotatoCactus.Network.Packets.In.NpcAttackPacket (npcAttackPacket)
import PotatoCactus.Network.Packets.In.ObjectActionPacket (objectActionPacket)
import PotatoCactus.Network.Packets.In.PlayerCommandPacket (commandPacket)
import PotatoCactus.Network.Packets.In.PlayerWalkPacket (playerMapWalk, playerWalkMessage)
import PotatoCactus.Network.Packets.Opcodes (socketClosedOpcode)
import PotatoCactus.Network.Packets.Reader (InboundPacket (opcode))

mapPacket :: PlayerIndex -> String -> InboundPacket -> Maybe GameChannelMessage
mapPacket playerId clientIdentifier packet =
  case opcode packet of
    4 -> Just $ playerChatMessage clientIdentifier packet
    17 -> npcActionPacket playerId packet
    18 -> npcActionPacket playerId packet
    21 -> npcActionPacket playerId packet
    40 -> Just $ continueDialoguePacket playerId packet
    41 -> Just $ equipItemPacket clientIdentifier packet
    70 -> Just $ objectActionPacket playerId packet
    72 -> Just $ npcAttackPacket playerId packet
    73 -> Just $ objectActionPacket playerId packet
    98 -> Just $ playerWalkMessage clientIdentifier packet -- Red X walk
    103 -> Just $ commandPacket playerId packet
    132 -> Just $ objectActionPacket playerId packet
    145 -> itemContainerClickPacket clientIdentifier packet
    155 -> npcActionPacket playerId packet
    164 -> Just $ playerWalkMessage clientIdentifier packet -- Yellow X walk
    185 -> Just $ buttonClickMessage playerId packet -- Interface button
    248 -> Just $ playerMapWalk clientIdentifier packet -- Minimap walk
    252 -> Just $ objectActionPacket playerId packet
    254 -> Just $ UnregisterClientMessage clientIdentifier
    _ -> Nothing
