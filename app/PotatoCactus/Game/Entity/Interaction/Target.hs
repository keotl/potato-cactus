module PotatoCactus.Game.Entity.Interaction.Target where

import PotatoCactus.Game.Entity.Npc.Npc (Npc, NpcIndex)
import PotatoCactus.Game.Entity.Object.GameObjectKey (GameObjectKey)
import PotatoCactus.Game.Message.ItemOnObjectPayload (ItemOnObjectPayload)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position, isNextTo, isWithin)

data InteractionTarget
  = ObjectTarget GameObjectKey (Either Int ItemOnObjectPayload)
  | NpcTarget NpcIndex NpcInteractionType
  | None
  deriving (Show)

data NpcInteractionType = NpcAttack | NpcAction Int deriving (Show)

-- TODO - Some entities are larger than 1 tile. e.g. trees.  - keotl 2023-03-15
-- TODO - For those entities, we have to somehow check the clickbox - keotl 2023-03-15
-- TODO - Some entities, e.g. stairs, can only start the interaction on from a single tile.  - keotl 2023-03-15
canStartInteractionFromPos :: Position -> Position -> Bool
canStartInteractionFromPos = isWithin 1
