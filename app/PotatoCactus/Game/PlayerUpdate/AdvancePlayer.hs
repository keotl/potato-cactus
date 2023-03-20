module PotatoCactus.Game.PlayerUpdate.AdvancePlayer where

import PotatoCactus.Game.Entity.Interaction.Interaction (advanceInteraction)
import PotatoCactus.Game.Entity.Npc.Npc (Npc, NpcIndex)
import PotatoCactus.Game.ItemContainer (ItemContainer (updated))
import PotatoCactus.Game.Movement.MovementEntity (isStopped)
import PotatoCactus.Game.Player (Player (..))
import PotatoCactus.Game.PlayerUpdate.ProcessPlayerUpdate (processPlayerUpdate)
import PotatoCactus.Game.Position (getPosition)
import PotatoCactus.Game.Typing (Advance (advance))

advancePlayer :: (NpcIndex -> Maybe Npc) -> Player -> Player
advancePlayer findNpc p =
  if skipUpdate_ p
    then p {skipUpdate_ = False}
    else
      let updated =
            foldl
              processPlayerUpdate
              (clearTransientProperties_ p)
              (pendingUpdates p)
       in let (updatedInteraction, pathOverride) =
                advanceInteraction
                  findNpc
                  (interaction updated)
                  (getPosition updated, isStopped . movement $ updated)
           in case pathOverride of
                [] ->
                  updated
                    { movement = advance (movement updated),
                      inventory = advance (inventory updated),
                      equipment = advance (equipment updated),
                      interaction = updatedInteraction,
                      pendingUpdates = []
                    }
                newPath ->
                  updated
                    { movement = advance (movement updated), -- TODO - interpolate path and issue walk command  - keotl 2023-03-20
                      inventory = advance (inventory updated),
                      equipment = advance (equipment updated),
                      interaction = updatedInteraction,
                      pendingUpdates = []
                    }

clearTransientProperties_ :: Player -> Player
clearTransientProperties_ p =
  p
    { chatMessage = Nothing,
      updateMask = 0
    }
