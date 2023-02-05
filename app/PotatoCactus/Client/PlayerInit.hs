module PotatoCactus.Client.PlayerInit where

import Data.Binary.BitPut (BitPut, putByteString, putNBits)
import Data.Binary.Put ()
import Data.ByteString (ByteString)
import PotatoCactus.Client.PlayerUpdate (toWord_)
import PotatoCactus.Game.Interface.GameTabs (TabDefinition (defaultTab, id), allTabs, combatTab)
import PotatoCactus.Game.Interface.PlayerInteraction (followInteraction, tradeInteraction)
import PotatoCactus.Game.ItemContainer (playerEquipment, playerInventory)
import PotatoCactus.Game.World
import PotatoCactus.Network.Packets.Out.PlayerInteractionPacket (showPlayerInteractionPacket)
import PotatoCactus.Network.Packets.Out.TabInterfacePacket (tabInterfacePacket)
import PotatoCactus.Network.Packets.Out.UpdateItemContainerPacket (updateItemContainerPacket)
import PotatoCactus.Network.Packets.Out.UpdateRunEnergyPacket (updateRunEnergyPacket)
import Prelude hiding (id)

playerInit :: ClientHandle -> World -> BitPut
playerInit client world = do
  -- reset tabs (opcode 71)
  mapM_ resetTab_ allTabs

  -- player options (opcode 104)
  putByteString $ showPlayerInteractionPacket followInteraction
  putByteString $ showPlayerInteractionPacket tradeInteraction

  -- attach equipment bonuses interface text  (opcode 126)
  -- TODO - can we skip ?  - keotl 2023-02-05

  -- update inventories (opcode 53)
  putByteString $ updateItemContainerPacket playerInventory
  putByteString $ updateItemContainerPacket playerEquipment

  -- run energy (opcode 110)
  putByteString $ updateRunEnergyPacket 100
  
  -- initialize player membership and index on server (opcode 249)

  -- set skill levels (opcode 134)

  -- send welcome messages (opcode 253)

  -- set friend list loaded (opcode 221)

  -- todo remove sentinel
  putNBits 0 $ toWord_ 0

resetTab_ :: TabDefinition -> BitPut
resetTab_ tab =
  Data.Binary.BitPut.putByteString $ tabInterfacePacket (id tab) (defaultTab tab)
