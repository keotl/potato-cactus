module PotatoCactus.Game.Entity.Npc.Npc where

import Data.Bits (Bits ((.&.)), (.|.))
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity)
import qualified PotatoCactus.Game.Combat.CombatEntity as CombatEntity
import PotatoCactus.Game.Combat.Hit (Hit)
import PotatoCactus.Game.Definitions.NpcDefinitions (NpcDefinition (hitpoints), NpcDefinitionId, npcDefinition)
import qualified PotatoCactus.Game.Entity.Animation.Animation as Anim
import PotatoCactus.Game.Entity.Npc.NpcMovement (NpcMovement, create)
import PotatoCactus.Game.Entity.Npc.NpcUpdateMask (NpcUpdateMask, npcAnimationUpdateFlag, npcForcedChatUpdateFlag, npcPrimaryHealthUpdateFlag, npcSecondaryHealthUpdateFlag)
import PotatoCactus.Game.Entity.Npc.RespawnStrategy (RespawnStrategy (Never), respawning, shouldDiscardEntity)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position)
import PotatoCactus.Game.Typing (Advance (advance), IsEntityActive (isEntityActive), Keyable (key), ShouldDiscard (shouldDiscard))
import PotatoCactus.Utils.Flow ((|>))

type NpcIndex = Int

data Npc = Npc
  { serverIndex :: NpcIndex,
    movement :: NpcMovement,
    updateMask :: NpcUpdateMask,
    definitionId :: NpcDefinitionId,
    animation :: Maybe Anim.Animation,
    forcedChat :: Maybe String,
    combat :: CombatEntity,
    respawn :: RespawnStrategy
  }
  deriving (Show)

instance GetPosition Npc where
  getPosition = getPosition . movement

instance Keyable Npc where
  key n = (show . definitionId $ n) ++ (show . serverIndex $ n)

create :: NpcDefinitionId -> Position -> RespawnStrategy -> Npc
create definitionId pos respawnStrategy =
  let defaultValues =
        Npc
          { serverIndex = -1,
            movement = PotatoCactus.Game.Entity.Npc.NpcMovement.create pos,
            updateMask = 0,
            definitionId = definitionId,
            combat = CombatEntity.create 1,
            animation = Nothing,
            forcedChat = Nothing,
            respawn = respawnStrategy
          }
   in case npcDefinition definitionId of
        Just def ->
          defaultValues
            { combat = CombatEntity.create (hitpoints def)
            }
        Nothing ->
          defaultValues

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

setAnimation :: Npc -> Anim.Animation -> Npc
setAnimation npc anim =
  npc
    { animation = Anim.setAnimation (animation npc) anim,
      updateMask = updateMask npc .|. npcAnimationUpdateFlag
    }

setForcedChat :: Npc -> String -> Npc
setForcedChat npc msg =
  npc
    { forcedChat = Just msg,
      updateMask = updateMask npc .|. npcForcedChatUpdateFlag
    }

instance IsEntityActive Npc where
  isEntityActive npc =
    case npc |> combat |> CombatEntity.state of
      CombatEntity.Dead -> False
      _ -> True

instance ShouldDiscard Npc where
  shouldDiscard npc =
    (shouldDiscardEntity . respawn $ npc)
      && (not . isEntityActive $ npc)
