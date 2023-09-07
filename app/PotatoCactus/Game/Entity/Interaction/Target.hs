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

-- TODO - Some entities are larger than 1 tile. e.g. trees.  - keotl 2023-03-15
-- TODO - For those entities, we have to somehow check the clickbox - keotl 2023-03-15
-- TODO - Some entities, e.g. stairs, can only start the interaction on from a single tile.  - keotl 2023-03-15
canStartInteractionFromPos :: Position -> Position -> Bool
canStartInteractionFromPos = isWithin 1
