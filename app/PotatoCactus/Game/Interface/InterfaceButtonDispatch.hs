module PotatoCactus.Game.Interface.InterfaceButtonDispatch where

import qualified PotatoCactus.Game.Movement.MovementEntity as PM
import PotatoCactus.Game.Player (PlayerIndex)
import qualified PotatoCactus.Game.Player as P
import PotatoCactus.Game.World (World, updatePlayerByIndex)

dispatchInterfaceButtonClick :: World -> PlayerIndex -> Int -> World
dispatchInterfaceButtonClick world playerIndex 152 =
  updatePlayerByIndex
    world
    playerIndex
    (\p -> (p {P.movement = PM.setRunning (P.movement p) False}))
dispatchInterfaceButtonClick world playerIndex 153 =
  updatePlayerByIndex
    world
    playerIndex
    (\p -> (p {P.movement = PM.setRunning (P.movement p) True}))
dispatchInterfaceButtonClick world _ _ = world
