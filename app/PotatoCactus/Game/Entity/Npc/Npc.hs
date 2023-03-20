module PotatoCactus.Game.Entity.Npc.Npc where

import PotatoCactus.Game.Entity.Npc.NpcMovement (NpcMovement, create)
import PotatoCactus.Game.Entity.Npc.NpcUpdateMask (NpcUpdateMask)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position)
import PotatoCactus.Game.Typing (Advance (advance), Keyable (key))

type NpcIndex = Int

type NpcDefinitionId = Int

data Npc = Npc
  { serverIndex :: NpcIndex,
    movement :: NpcMovement,
    updateMask :: NpcUpdateMask,
    definitionId :: NpcDefinitionId
  }
  deriving (Show)

instance GetPosition Npc where
  getPosition = getPosition . movement

instance Keyable Npc where
  key n = (show . definitionId $ n) ++ (show . serverIndex $ n)

instance Advance Npc where
  advance npc =
    npc
      { movement = advance . movement $ npc
      }

create :: NpcDefinitionId -> Position -> Npc
create definitionId pos =
  Npc
    { serverIndex = -1,
      movement = PotatoCactus.Game.Entity.Npc.NpcMovement.create pos,
      updateMask = 0,
      definitionId = definitionId
    }
