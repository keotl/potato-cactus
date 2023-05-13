module PotatoCactus.Game.PlayerUpdate.PlayerUpdate where

import PotatoCactus.Game.Definitions.EquipmentDefinitions (EquipmentSlot)
import PotatoCactus.Game.Entity.Interaction.Target (NpcInteractionType)
import PotatoCactus.Game.Entity.Npc.Npc (NpcIndex)
import PotatoCactus.Game.Message.EquipItemMessagePayload (EquipItemMessagePayload)
import PotatoCactus.Game.Message.ObjectClickPayload (ObjectClickPayload)
import PotatoCactus.Game.PlayerUpdate.ChatMessage (ChatMessage)
import PotatoCactus.Game.Message.ItemOnObjectPayload (ItemOnObjectPayload)

data PlayerUpdate
  = EquipItem EquipItemMessagePayload
  | UnequipItem EquipmentSlot
  | UpdateAppearanceModel Int Int
  | SayChatMessage ChatMessage
  | SayForcedChatMessage String
  | InteractWithObject ObjectClickPayload
  | InteractWithObjectWithItem ItemOnObjectPayload
  | InteractWithNpc NpcIndex NpcInteractionType
  | ContinueDialogue
  deriving (Show)
