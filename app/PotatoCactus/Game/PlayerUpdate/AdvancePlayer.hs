module PotatoCactus.Game.PlayerUpdate.AdvancePlayer where

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
import PotatoCactus.Game.Position (getPosition)
import PotatoCactus.Game.Typing (Advance (advance))
import PotatoCactus.Utils.Flow ((|>))

advancePlayer :: (NpcIndex -> Maybe Npc) -> Player -> Player
advancePlayer findNpc p =
  if skipUpdate_ p
    then p {skipUpdate_ = False}
    else
      let updated =
            p |> processPendingUpdates_
              |> commonUpdates_
       in let updatedInteraction =
                advanceInteraction
                  findNpc
                  (interaction updated)
                  (getPosition updated, isStopped . movement $ updated)
           in case (state updatedInteraction, target updatedInteraction) of
                (PendingPathing, NpcTarget npcId _) ->
                  case findNpc npcId of
                    Nothing ->
                      updated
                        { movement = advance (movement updated),
                          interaction = create,
                          pendingUpdates = []
                        }
                    Just npc ->
                      let desiredPath = findPathNaive 666 (getPosition p) (getPosition npc)
                       in updated
                            { movement = Movement.immediatelyQueueMovement (movement updated) desiredPath,
                              interaction = updatedInteraction,
                              pendingUpdates = []
                            }
                _ ->
                  updated
                    { movement = advance (movement updated),
                      interaction = updatedInteraction,
                      pendingUpdates = []
                    }

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
      interfaces = advance . interfaces $ p,
      combat = advance . combat $ p
    }
