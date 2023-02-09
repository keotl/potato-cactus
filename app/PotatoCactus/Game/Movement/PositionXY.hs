module PotatoCactus.Game.Movement.PositionXY where

import qualified PotatoCactus.Game.Position as Position (Position, x, y)

data PositionXY = PositionXY
  { x :: Int,
    y :: Int
  }

toXY :: Position.Position -> PositionXY
toXY pos =
  PositionXY {x = Position.x pos, y = Position.y pos}
