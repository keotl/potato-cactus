module PotatoCactus.Game.Entity.Interaction.Interaction where

import PotatoCactus.Game.Entity.Interaction.State (InteractionState (InProgress, Pending))
import PotatoCactus.Game.Entity.Interaction.Target (InteractionTarget (None), canStartInteractionFromPos)
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

advanceInteraction :: Interaction -> (Position, Bool) -> Interaction
advanceInteraction (Interaction target Pending) (pos, isStopped) =
  case (isStopped, canStartInteractionFromPos target pos) of
    (True, True) -> Interaction target InProgress
    (False, _) -> Interaction target Pending
    _ -> Interaction None Pending
advanceInteraction interaction _ = interaction
