module PotatoCactus.Game.Entity.Interaction.Target where

import PotatoCactus.Game.Definitions.Types.ItemDefinition (ItemId)
import PotatoCactus.Game.Entity.Npc.Npc (Npc, NpcIndex)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject)
import PotatoCactus.Game.Message.ItemOnObjectPayload (ItemOnObjectPayload)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position, isWithin)
import PotatoCactus.Game.Scripting.Actions.CreateInterface (WidgetId)

data InteractionTarget
  = ObjectTarget GameObject ObjectInteractionType
  | NpcTarget NpcIndex NpcInteractionType
  | GroundItemTarget ItemId Int Position GroundItemInteractionType
  | None
  deriving (Show, Eq)

data ObjectInteractionType = ObjectAction Int | ItemOnObject WidgetId Int ItemId deriving (Show, Eq)

data NpcInteractionType = NpcAttack | NpcAction Int deriving (Show, Eq)

data GroundItemInteractionType = ItemPickup deriving (Show, Eq)
