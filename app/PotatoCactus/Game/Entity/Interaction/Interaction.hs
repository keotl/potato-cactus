module PotatoCactus.Game.Entity.Interaction.Interaction where

import PotatoCactus.Game.Entity.Interaction.State (InteractionState (InProgress, Pending, PendingPathing))
import PotatoCactus.Game.Entity.Interaction.Target (GroundItemInteractionType (ItemPickup), InteractionTarget (GroundItemTarget, None, NpcTarget, ObjectTarget), canStartInteractionFromPos)
import PotatoCactus.Game.Entity.Npc.Npc (Npc, NpcIndex)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position, isNextTo)

data Interaction = Interaction
  { target :: InteractionTarget,
    state :: InteractionState
  }
  deriving (Show)

create :: Interaction
create = Interaction {target = None, state = Pending}

createForTarget :: InteractionTarget -> Interaction
createForTarget target =
  create {target = target}

advanceInteraction :: (NpcIndex -> Maybe Npc) -> Interaction -> (Position, Bool) -> Interaction
advanceInteraction findNpc (Interaction (NpcTarget npcId interactionType) Pending) (pos, isStopped) =
  case findNpc npcId of
    Nothing -> Interaction None Pending
    Just npc -> case (isStopped, canStartInteractionFromPos (getPosition npc) pos) of
      (True, True) -> Interaction (NpcTarget npcId interactionType) InProgress
      (True, False) -> Interaction (NpcTarget npcId interactionType) PendingPathing
      _ -> Interaction (NpcTarget npcId interactionType) Pending
advanceInteraction findNpc (Interaction (NpcTarget npcId interactionType) PendingPathing) (pos, isStopped) =
  case findNpc npcId of
    Nothing -> Interaction None Pending
    Just npc -> case (isStopped, canStartInteractionFromPos (getPosition npc) pos) of
      (_, True) -> Interaction (NpcTarget npcId interactionType) InProgress
      _ -> Interaction (NpcTarget npcId interactionType) PendingPathing
advanceInteraction _ (Interaction (ObjectTarget objectKey actionIndex) Pending) (pos, isStopped) =
  -- TODO - Handle case where the target object is removed while the interaction is in progress.  - keotl 2023-09-05
  case (isStopped, canStartInteractionFromPos (getPosition objectKey) pos) of
    (True, True) -> Interaction (ObjectTarget objectKey actionIndex) InProgress
    (False, _) -> Interaction (ObjectTarget objectKey actionIndex) Pending
    _ -> Interaction None Pending
advanceInteraction _ (Interaction (GroundItemTarget itemId quantity position ItemPickup) Pending) (pos, isStopped) =
  case (isStopped, position == pos) of
    (True, True) -> Interaction (GroundItemTarget itemId quantity position ItemPickup) InProgress
    (False, _) -> Interaction (GroundItemTarget itemId quantity position ItemPickup) Pending
    _ -> Interaction None Pending
advanceInteraction _ interaction _ = interaction
