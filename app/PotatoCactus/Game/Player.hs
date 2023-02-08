module PotatoCactus.Game.Player where

import PotatoCactus.Game.Movement.MovementEntity (MovementEntity)
import PotatoCactus.Game.Position (GetPosition (getPosition))

data Player = Player
  { username :: String,
    movement :: MovementEntity
  }
  deriving (Show)

-- e = Player {username = "foo", movement = StaticMovement {position_ = }}

instance GetPosition Player where
  getPosition = getPosition . movement
