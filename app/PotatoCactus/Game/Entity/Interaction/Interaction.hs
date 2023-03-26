module PotatoCactus.Game.Entity.Interaction.Interaction where

import PotatoCactus.Game.Entity.Interaction.State (InteractionState (InProgress, Pending, PendingPathing))
import PotatoCactus.Game.Entity.Interaction.Target (InteractionTarget (None, NpcTarget, ObjectTarget), canStartInteractionFromPos)
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
      (True, False) -> Interaction None PendingPathing
      _ -> Interaction (NpcTarget npcId interactionType) Pending
advanceInteraction _ (Interaction (ObjectTarget objectKey actionIndex) Pending) (pos, isStopped) =
  case (isStopped, canStartInteractionFromPos (getPosition objectKey) pos) of
    (True, True) -> Interaction (ObjectTarget objectKey actionIndex) InProgress
    (False, _) -> Interaction (ObjectTarget objectKey actionIndex) Pending
    _ -> Interaction None Pending
advanceInteraction _ interaction _ = interaction
