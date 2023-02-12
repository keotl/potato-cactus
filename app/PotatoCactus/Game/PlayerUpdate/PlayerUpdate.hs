module PotatoCactus.Game.PlayerUpdate.PlayerUpdate where
import PotatoCactus.Game.PlayerUpdate.ChatMessage (ChatMessage)

data PlayerUpdate
  = EquipItem Int
  | UnequipItem Int
  | UpdateAppearanceModel Int Int
  | SayChatMessage ChatMessage
  | SayForcedChatMessage String
  deriving (Show)
