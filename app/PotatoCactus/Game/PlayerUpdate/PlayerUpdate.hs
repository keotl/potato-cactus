module PotatoCactus.Game.PlayerUpdate.PlayerUpdate where

import PotatoCactus.Game.Definitions.EquipmentDefinitions (EquipmentSlot)
import PotatoCactus.Game.Message.EquipItemMessagePayload
import PotatoCactus.Game.PlayerUpdate.ChatMessage (ChatMessage)

data PlayerUpdate
  = EquipItem EquipItemMessagePayload
  | UnequipItem EquipmentSlot
  | UpdateAppearanceModel Int Int
  | SayChatMessage ChatMessage
  | SayForcedChatMessage String
  deriving (Show)
