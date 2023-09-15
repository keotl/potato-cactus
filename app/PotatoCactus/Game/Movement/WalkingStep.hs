module PotatoCactus.Game.Movement.WalkingStep where

import PotatoCactus.Game.Movement.PositionXY (PositionXY (PositionXY))
import qualified PotatoCactus.Game.Movement.PositionXY as PositionXY
import PotatoCactus.Game.Position (Position (x, y))

data WalkingStep = WalkingStep Int Int deriving (Show)

toPosition :: Position -> WalkingStep -> Position
toPosition reference (WalkingStep dx dy) =
  reference
    { x = x reference + capDelta_ dx,
      y = y reference + capDelta_ dy
    }

toPositionXY :: PositionXY -> WalkingStep -> PositionXY
toPositionXY reference (WalkingStep dx dy) =
  reference
    { PositionXY.x = PositionXY.x reference + capDelta_ dx,
      PositionXY.y = PositionXY.y reference + capDelta_ dy
    }

capDelta_ :: Int -> Int
capDelta_ x
  | x > 1 = 1
  | x < -1 = -1
  | otherwise = x
