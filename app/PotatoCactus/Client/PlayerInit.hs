module PotatoCactus.Client.PlayerInit where

import Data.Binary.BitPut (BitPut, putByteString, putNBits)
import Data.Binary.Put ()
import Data.ByteString (ByteString)
import PotatoCactus.Client.PlayerUpdate (toWord_)
import PotatoCactus.Game.Interface.GameTabs (TabDefinition (defaultTab, id), allTabs, combatTab)
import PotatoCactus.Game.Interface.PlayerInteraction (followInteraction, tradeInteraction)
import PotatoCactus.Game.World
import PotatoCactus.Network.Packets.Out.PlayerInteractionPacket (showPlayerInteractionPacket)
import PotatoCactus.Network.Packets.Out.TabInterfacePacket (tabInterfacePacket)
import Prelude hiding (id)

playerInit :: ClientHandle -> World -> BitPut
playerInit client world = do
  -- reset tabs (opcode 71)
  mapM_ resetTab_ allTabs

  -- player options (opcode 104)
  putByteString $ showPlayerInteractionPacket followInteraction
  putByteString $ showPlayerInteractionPacket tradeInteraction

-- attach interface text ?? (opcode 126)

-- run energy (opcode 110)

-- initialize player membership and index on server (opcode 249)

-- set skill levels (opcode 134)

-- send welcome messages (opcode 253)

-- set friend list loaded (opcode 221)

resetTab_ :: TabDefinition -> BitPut
resetTab_ tab =
  Data.Binary.BitPut.putByteString $ tabInterfacePacket (id tab) (defaultTab tab)
