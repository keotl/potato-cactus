module PotatoCactus.Game.Entity.Interaction.Target where

import PotatoCactus.Game.Entity.Object.GameObjectKey (GameObjectKey)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position, isNextTo, isWithin)

data InteractionTarget
  = ObjectTarget GameObjectKey Int
  | MobTarget -- TODO - keotl 2023-03-14
  | None
  deriving (Show)

-- TODO - Some entities are larger than 1 tile. e.g. trees.  - keotl 2023-03-15
-- TODO - For those entities, we have to somehow check the clickbox - keotl 2023-03-15
-- TODO - Some entities, e.g. stairs, can only start the interaction on from a single tile.  - keotl 2023-03-15
canStartInteractionFromPos :: InteractionTarget -> Position -> Bool
canStartInteractionFromPos (ObjectTarget obj _) p =
  isWithin 1 (getPosition obj) p
canStartInteractionFromPos _ _ = False
