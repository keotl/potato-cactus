module PotatoCactus.Client.PlayerInit where

import Data.Binary.BitPut (BitPut, putByteString, putNBits)
import Data.Binary.Put ()
import Data.ByteString (ByteString)
import PotatoCactus.Game.Interface.FriendsList (FriendsListStatus (Loading))
import PotatoCactus.Game.Interface.GameTabs (TabDefinition (defaultTab, id), allTabs, combatTab)
import PotatoCactus.Game.Interface.PlayerInteraction (followInteraction, tradeInteraction)
import PotatoCactus.Game.Interface.PlayerSettings (BrightnessLevel (Brightness4), MouseType (TwoButtons), PlayerSettings (PlayerSettings), VolumeLevel (Volume4))
import PotatoCactus.Game.ItemContainer (playerEquipment, playerInventory)
import PotatoCactus.Game.Player (Player)
import PotatoCactus.Game.Position (Position (Position))
import qualified PotatoCactus.Game.Skills as SK
import PotatoCactus.Game.World
import PotatoCactus.Network.Packets.Out.ChatboxMessagePacket (chatboxMessagePacket)
import PotatoCactus.Network.Packets.Out.InitializePlayerPacket (initializePlayerPacket)
import PotatoCactus.Network.Packets.Out.LoadMapRegionPacket (loadMapRegionPacket)
import PotatoCactus.Network.Packets.Out.PlayerInteractionPacket (showPlayerInteractionPacket)
import PotatoCactus.Network.Packets.Out.PlayerSettingsPackets (allPlayerSettingsPackets)
import PotatoCactus.Network.Packets.Out.TabInterfacePacket (tabInterfacePacket)
import PotatoCactus.Network.Packets.Out.UpdateFriendsListStatusPacket (updateFriendsListStatusPacket)
import PotatoCactus.Network.Packets.Out.UpdateItemContainerPacket (updateItemContainerPacket)
import PotatoCactus.Network.Packets.Out.UpdateRunEnergyPacket (updateRunEnergyPacket)
import PotatoCactus.Network.Packets.Out.UpdateSkillPacket (updateSkillPacket)
import Prelude hiding (id)

playerInit :: ClientHandle -> BitPut
playerInit client = do
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
  putByteString $ initializePlayerPacket True 0

  -- set skill levels (opcode 134)
  mapM_ sendSkill_ mockSkills_

  -- send welcome messages (opcode 253)
  putByteString $ chatboxMessagePacket "Welcome to PotatoCactus."

  -- set friend list loaded (opcode 221)
  putByteString $ updateFriendsListStatusPacket Loading

  -- send config (opcode 36) (show.kts)
  putByteString $ allPlayerSettingsPackets mockSettings_

  -- load initial map region (opcode 73) (could move to main loop)
  putByteString $ loadMapRegionPacket mockPosition_

resetTab_ :: TabDefinition -> BitPut
resetTab_ tab =
  Data.Binary.BitPut.putByteString $ tabInterfacePacket (id tab) (defaultTab tab)

mockSkills_ :: [SK.Skill]
mockSkills_ =
  map mockSkill_ SK.allSkills

mockSkill_ :: SK.SkillDefinition -> SK.Skill
mockSkill_ def =
  SK.Skill {SK.skill = def, SK.experience = 123}

sendSkill_ :: SK.Skill -> BitPut
sendSkill_ skill = putByteString $ updateSkillPacket skill

mockSettings_ :: PlayerSettings
mockSettings_ = PlayerSettings Brightness4 TwoButtons True True True Volume4 Volume4 False True

mockPosition_ :: Position
mockPosition_ = Position 3093 3244 0
