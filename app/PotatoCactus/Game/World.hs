module PotatoCactus.Game.World where

import PotatoCactus.Game.Player (Player)

data World = World
  { players :: [Player]
  }

defaultWorldValue = World {players = []}
