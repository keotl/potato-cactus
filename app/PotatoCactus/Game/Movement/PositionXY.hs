module PotatoCactus.Game.Movement.PositionXY where

import qualified PotatoCactus.Game.Position as Position (Position (Position), x, y, z)

data PositionXY = PositionXY
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq)

toXY :: Position.Position -> PositionXY
toXY pos =
  PositionXY {x = Position.x pos, y = Position.y pos}

fromXY :: PositionXY -> Int -> Position.Position
fromXY pos plane =
  Position.Position
    { Position.x = x pos,
      Position.y = y pos,
      Position.z = plane
    }
