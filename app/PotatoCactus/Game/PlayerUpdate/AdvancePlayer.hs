module PotatoCactus.Game.PlayerUpdate.AdvancePlayer where

import PotatoCactus.Game.Entity.Interaction.AdvanceInteractionDeps (AdvanceInteractionSelectors, locateInteractionTarget)
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction (state, target), advanceInteraction, create)
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (PendingPathing))
import PotatoCactus.Game.Entity.Interaction.Target (InteractionTarget (NpcTarget))
import PotatoCactus.Game.Entity.Npc.Npc (Npc, NpcIndex)
import PotatoCactus.Game.ItemContainer (ItemContainer (updated))
import PotatoCactus.Game.Movement.MovementEntity (isStopped)
import qualified PotatoCactus.Game.Movement.MovementEntity as Movement
import PotatoCactus.Game.Movement.PathPlanner (findPathNaive)
import PotatoCactus.Game.Player (Player (..))
import PotatoCactus.Game.PlayerUpdate.ProcessPlayerUpdate (processPlayerUpdate)
import PotatoCactus.Game.Position (Position, getPosition)
import PotatoCactus.Game.Typing (Advance (advance))
import PotatoCactus.Utils.Flow ((|>))

advancePlayer :: AdvanceInteractionSelectors -> Player -> Player
advancePlayer advanceInteractionDeps p =
  if skipUpdate_ p
    then p {skipUpdate_ = False}
    else
      let updated =
            p |> processPendingUpdates_
              |> commonUpdates_
       in updated
            { movement = advance (movement updated),
              interaction =
                advanceInteraction_
                  advanceInteractionDeps
                  (isStopped . movement $ p)
                  (getPosition p)
                  (interaction p),
              pendingUpdates = []
            }

advanceInteraction_ :: AdvanceInteractionSelectors -> Bool -> Position -> Interaction -> Interaction
advanceInteraction_ _ False _ = id
advanceInteraction_ deps True playerPos =
  advanceInteraction (locateInteractionTarget deps playerPos)

clearTransientProperties_ :: Player -> Player
clearTransientProperties_ p =
  p
    { chatMessage = Nothing,
      updateMask = 0,
      animation = Nothing,
      chatboxMessages = []
    }

processPendingUpdates_ :: Player -> Player
processPendingUpdates_ p =
  foldl
    processPlayerUpdate
    (clearTransientProperties_ p)
    (pendingUpdates p)

commonUpdates_ :: Player -> Player
commonUpdates_ p =
  p
    { inventory = advance . inventory $ p,
      equipment = advance . equipment $ p,
      combat = advance . combat $ p,
      interfaces = advance . interfaces $ p
    }
