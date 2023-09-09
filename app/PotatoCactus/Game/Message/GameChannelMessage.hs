module PotatoCactus.Game.Message.GameChannelMessage where

import PotatoCactus.Game.Definitions.EquipmentDefinitions (EquipmentSlot)
import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectId)
import PotatoCactus.Game.Definitions.Types.ItemDefinition (ItemId)
import PotatoCactus.Game.Entity.Npc.Npc (NpcIndex)
import PotatoCactus.Game.Message.EquipItemMessagePayload (EquipItemMessagePayload)
import PotatoCactus.Game.Message.ItemOnObjectPayload (ItemOnObjectPayload)
import PotatoCactus.Game.Message.ObjectClickPayload (ObjectClickPayload)
import PotatoCactus.Game.Message.RegisterClientPayload (RegisterClientPayload)
import PotatoCactus.Game.Movement.PositionXY (PositionXY)
import PotatoCactus.Game.Movement.WalkingStep (WalkingStep)
import PotatoCactus.Game.Player (PlayerIndex)
import PotatoCactus.Game.PlayerUpdate.ChatMessage (ChatMessage)
import PotatoCactus.Game.Scripting.Actions.CreateInterface (WidgetId)

data GameChannelMessage
  = RegisterClientMessage RegisterClientPayload
  | UnregisterClientMessage String
  | PlayerWalkMessage String PositionXY Bool [WalkingStep]
  | PlayerCommandMessage PlayerIndex String [String]
  | InterfaceButtonClickMessage PlayerIndex Int
  | PlayerChatMessage String ChatMessage
  | PlayerContinueDialogueMessage PlayerIndex Int
  | EquipItemMessage String EquipItemMessagePayload
  | UnequipItemMessage String EquipmentSlot
  | ObjectClickMessage PlayerIndex GameObjectId PositionXY Int
  | ItemOnObjectMessage PlayerIndex WidgetId GameObjectId PositionXY Int ItemId
  | NpcAttackMessage PlayerIndex NpcIndex
  | NpcClickMessage PlayerIndex NpcIndex Int
  | DropItemMessage PlayerIndex WidgetId ItemId Int
  | PickupGroundItemMessage PlayerIndex ItemId PositionXY
  | UpdateWorldMessage
