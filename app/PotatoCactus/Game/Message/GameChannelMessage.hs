module PotatoCactus.Game.Message.GameChannelMessage where

import PotatoCactus.Game.Definitions.EquipmentDefinitions (EquipmentSlot)
import PotatoCactus.Game.Message.EquipItemMessagePayload (EquipItemMessagePayload)
import PotatoCactus.Game.Message.RegisterClientPayload (RegisterClientPayload)
import PotatoCactus.Game.Movement.PositionXY (PositionXY)
import PotatoCactus.Game.Movement.WalkingStep (WalkingStep)
import PotatoCactus.Game.PlayerUpdate.ChatMessage (ChatMessage)
import PotatoCactus.Game.Message.ObjectClickPayload (ObjectClickPayload)
import PotatoCactus.Game.Player (PlayerIndex)

data GameChannelMessage
  = RegisterClientMessage RegisterClientPayload
  | UnregisterClientMessage String
  | PlayerWalkMessage String PositionXY Bool [WalkingStep]
  | InterfaceButtonClickMessage String Int
  | PlayerChatMessage String ChatMessage
  | EquipItemMessage String EquipItemMessagePayload
  | UnequipItemMessage String EquipmentSlot
  | ObjectClickMessage PlayerIndex ObjectClickPayload
  | UpdateWorldMessage
