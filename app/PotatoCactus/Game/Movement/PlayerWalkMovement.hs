module PotatoCactus.Game.Movement.PlayerWalkMovement where

import PotatoCactus.Game.Position (GetPosition, Position, getPosition)
import PotatoCactus.Game.Typing (Advance (advance))

data PlayerWalkMovement = PlayerWalkMovement
  { position_ :: Position,
    queue_ :: [Position]
  }
  deriving (Show)

instance GetPosition PlayerWalkMovement where
  getPosition = position_

instance Advance PlayerWalkMovement where
  advance m =
    case queue_ m of
      [] -> PlayerWalkMovement {position_ = position_ m, queue_ = []}
      (x : xs) -> PlayerWalkMovement {position_ = x, queue_ = xs}

