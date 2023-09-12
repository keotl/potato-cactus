module PotatoCactus.Game.PlayerUpdate.AdvancePlayer (advancePlayer) where

import PotatoCactus.Config.Constants (npcDisengageDistance)
import PotatoCactus.Game.Combat.AdvanceCombatEntity (advanceCombatEntity)
import PotatoCactus.Game.Combat.AdvanceCombatEntityDeps (AdvanceCombatEntityDeps)
import PotatoCactus.Game.Combat.LocateCombatTarget (LocateTargetArgs (LocateTargetArgs), locateCombatTarget)
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

advancePlayer :: AdvanceInteractionSelectors -> AdvanceCombatEntityDeps -> Player -> Player
advancePlayer advanceInteractionDeps advanceCombatDeps old =
  if skipUpdate_ old
    then old {skipUpdate_ = False}
    else
      let p =
            old
              |> clearTransientProperties_
              |> processPendingUpdates_
       in p
            { movement = advance (movement p),
              interaction =
                advanceInteraction_
                  advanceInteractionDeps
                  (isStopped . movement $ p)
                  (getPosition p)
                  (interaction p),
              inventory = advance . inventory $ p,
              equipment = advance . equipment $ p,
              combat =
                advanceCombatEntity
                  (locateCombatTarget advanceCombatDeps (LocateTargetArgs 1 npcDisengageDistance) (getPosition p))
                  (combat p),
              interfaces = advance . interfaces $ p,
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
      chatboxMessages = [],
      droppedItemIndices = []
    }

processPendingUpdates_ :: Player -> Player
processPendingUpdates_ p =
  foldl processPlayerUpdate p (pendingUpdates p)
