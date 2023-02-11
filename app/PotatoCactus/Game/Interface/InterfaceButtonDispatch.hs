module PotatoCactus.Game.Interface.InterfaceButtonDispatch where

import qualified PotatoCactus.Game.Movement.MovementEntity as PM
import qualified PotatoCactus.Game.Player as P
import PotatoCactus.Game.World (World, updatePlayer)

dispatchInterfaceButtonClick :: World -> String -> Int -> World
dispatchInterfaceButtonClick world playerName 152 =
  updatePlayer
    world
    playerName
    (\p -> (p {P.movement = PM.setRunning (P.movement p) False}))
dispatchInterfaceButtonClick world playerName 153 =
  updatePlayer
    world
    playerName
    (\p -> (p {P.movement = PM.setRunning (P.movement p) True}))
dispatchInterfaceButtonClick world _ _ = world
