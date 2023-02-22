module PotatoCactus.Game.PlayerUpdate.AdvancePlayer where

import PotatoCactus.Game.ItemContainer (ItemContainer (updated))
import PotatoCactus.Game.Player (Player (..))
import PotatoCactus.Game.PlayerUpdate.ProcessPlayerUpdate (processPlayerUpdate)
import PotatoCactus.Game.Typing (Advance (advance))

advancePlayer :: Player -> Player
advancePlayer p =
  if skipUpdate_ p
    then p {skipUpdate_ = False}
    else
      ( ( foldl
            processPlayerUpdate
            (clearTransientProperties_ p)
            (pendingUpdates p)
        )
          { movement = advance (movement p),
            inventory = advance (inventory p),
            equipment = advance (equipment p),
            pendingUpdates = []
          }
      )

clearTransientProperties_ :: Player -> Player
clearTransientProperties_ p =
  p
    { chatMessage = Nothing,
      updateMask = 0
    }
