module PotatoCactus.Game.Entity.Interaction.Interaction where

import PotatoCactus.Game.Entity.Interaction.State (InteractionState (InProgress, Pending, PendingPathing))
import PotatoCactus.Game.Entity.Interaction.Target (InteractionTarget (None))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position)

data Interaction = Interaction
  { target :: InteractionTarget,
    state :: InteractionState
  }
  deriving (Show, Eq)

create :: Interaction
create = Interaction {target = None, state = Pending}

createForTarget :: InteractionTarget -> Interaction
createForTarget target =
  create {target = target}

data InteractionTargetStatus = Distant | Adjacent | Removed deriving (Show, Eq)

type LocateInteractionTarget = InteractionTarget -> InteractionTargetStatus

advanceInteraction :: LocateInteractionTarget -> Interaction -> Interaction
advanceInteraction locateTarget interaction =
  case locateTarget . target $ interaction of
    Distant -> interaction {state = PendingPathing}
    Adjacent -> interaction {state = InProgress}
    Removed -> create
