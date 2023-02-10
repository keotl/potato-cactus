module PotatoCactus.Game.Movement.WalkingStep where

import PotatoCactus.Game.Movement.PositionXY (PositionXY (PositionXY))
import qualified PotatoCactus.Game.Movement.PositionXY as PositionXY
import PotatoCactus.Game.Position (Position (x, y))

data WalkingStep = WalkingStep Int Int

toPosition :: Position -> WalkingStep -> Position
toPosition reference (WalkingStep dx dy) =
  reference
    { x = x reference + dx,
      y = y reference + dy
    }

toPositionXY :: PositionXY -> WalkingStep -> PositionXY
toPositionXY reference (WalkingStep dx dy) =
  reference
    { PositionXY.x = PositionXY.x reference + dx,
      PositionXY.y = PositionXY.y reference + dy
    }
