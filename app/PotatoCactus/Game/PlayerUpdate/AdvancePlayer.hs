module PotatoCactus.Game.PlayerUpdate.AdvancePlayer where

import PotatoCactus.Game.Entity.Interaction.Interaction (advanceInteraction)
import PotatoCactus.Game.ItemContainer (ItemContainer (updated))
import PotatoCactus.Game.Movement.MovementEntity (isStopped)
import PotatoCactus.Game.Player (Player (..))
import PotatoCactus.Game.PlayerUpdate.ProcessPlayerUpdate (processPlayerUpdate)
import PotatoCactus.Game.Position (getPosition)
import PotatoCactus.Game.Typing (Advance (advance))

advancePlayer :: Player -> Player
advancePlayer p =
  if skipUpdate_ p
    then p {skipUpdate_ = False}
    else
      let updated =
            foldl
              processPlayerUpdate
              (clearTransientProperties_ p)
              (pendingUpdates p)
       in updated
            { movement = advance (movement updated),
              inventory = advance (inventory updated),
              equipment = advance (equipment updated),
              interaction =
                advanceInteraction
                  (interaction updated)
                  (getPosition updated, isStopped . movement $ updated),
              pendingUpdates = []
            }

clearTransientProperties_ :: Player -> Player
clearTransientProperties_ p =
  p
    { chatMessage = Nothing,
      updateMask = 0
    }
