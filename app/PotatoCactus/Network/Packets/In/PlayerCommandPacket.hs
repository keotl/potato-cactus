module PotatoCactus.Network.Packets.In.PlayerCommandPacket (commandPacket) where

import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (PlayerCommandMessage))
import PotatoCactus.Game.Player (PlayerIndex)
import PotatoCactus.Network.Binary (readStr)
import PotatoCactus.Network.Packets.Reader (InboundPacket (payload))

commandPacket :: PlayerIndex -> InboundPacket -> GameChannelMessage
commandPacket playerId packet =
  let (text, _) = readStr (payload packet)
   in let (cmd : args) = words text
       in PlayerCommandMessage playerId cmd args
