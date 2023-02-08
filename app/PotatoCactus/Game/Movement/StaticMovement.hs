module PotatoCactus.Game.Movement.StaticMovement where

import PotatoCactus.Game.Position (GetPosition (getPosition), Position)
import PotatoCactus.Game.Typing (Advance (advance))

data StaticMovement = StaticMovement
  { position_ :: Position
  }

instance GetPosition StaticMovement where
  getPosition = position_

instance Advance StaticMovement where
  advance m = m

instance Show StaticMovement where
  show = show . position_
