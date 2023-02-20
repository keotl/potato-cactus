module PotatoCactus.Game.PlayerUpdate.ProcessPlayerUpdate where

import PotatoCactus.Game.Player (Player (chatMessage, updateMask))
import PotatoCactus.Game.PlayerUpdate.PlayerUpdate (PlayerUpdate (SayChatMessage))
import PotatoCactus.Game.PlayerUpdate.UpdateMask (chatFlag)
import Data.Bits ((.|.))

processPlayerUpdate :: Player -> PlayerUpdate -> Player
processPlayerUpdate p (SayChatMessage message) =
  p {chatMessage = Just message, updateMask = updateMask p .|. chatFlag}
processPlayerUpdate p _ = p
