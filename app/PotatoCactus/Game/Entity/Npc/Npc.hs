module PotatoCactus.Game.Entity.Npc.Npc where

import Data.Bits (Bits ((.&.)), (.|.))
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity)
import qualified PotatoCactus.Game.Combat.CombatEntity as CombatEntity
import PotatoCactus.Game.Combat.Hit (Hit)
import PotatoCactus.Game.Definitions.NpcDefinitions (NpcDefinition (hitpoints), NpcDefinitionId, npcDefinition)
import PotatoCactus.Game.Entity.Npc.NpcMovement (NpcMovement, create)
import PotatoCactus.Game.Entity.Npc.NpcUpdateMask (NpcUpdateMask, npcPrimaryHealthUpdateFlag, npcSecondaryHealthUpdateFlag)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position)
import PotatoCactus.Game.Typing (Advance (advance), Keyable (key))

type NpcIndex = Int

data Npc = Npc
  { serverIndex :: NpcIndex,
    movement :: NpcMovement,
    updateMask :: NpcUpdateMask,
    definitionId :: NpcDefinitionId,
    combat :: CombatEntity
  }
  deriving (Show)

instance GetPosition Npc where
  getPosition = getPosition . movement

instance Keyable Npc where
  key n = (show . definitionId $ n) ++ (show . serverIndex $ n)

create :: NpcDefinitionId -> Position -> Npc
create definitionId pos =
  case npcDefinition definitionId of
    Just def ->
      Npc
        { serverIndex = -1,
          movement = PotatoCactus.Game.Entity.Npc.NpcMovement.create pos,
          updateMask = 0,
          definitionId = definitionId,
          combat = CombatEntity.create (hitpoints def)
        }
    Nothing ->
      Npc
        { serverIndex = -1,
          movement = PotatoCactus.Game.Entity.Npc.NpcMovement.create pos,
          updateMask = 0,
          definitionId = definitionId,
          combat = CombatEntity.create 1
        }

applyHit :: CombatEntity.CombatTarget -> Hit -> Npc -> Npc
applyHit target hit npc =
  npc
    { combat = CombatEntity.applyHit (combat npc) target hit,
      updateMask =
        if (updateMask npc .&. npcPrimaryHealthUpdateFlag) > 0
          then updateMask npc .|. npcSecondaryHealthUpdateFlag
          else updateMask npc .|. npcPrimaryHealthUpdateFlag
    }

-- Sets the attack cooldown based on definition
setAttackCooldown :: Npc -> Npc
setAttackCooldown npc =
  npc {combat = CombatEntity.setAttackCooldown (combat npc) 10}

setAttackTarget :: Npc -> CombatEntity.CombatTarget -> Npc
setAttackTarget npc target =
  npc {combat = CombatEntity.setTarget (combat npc) target}
