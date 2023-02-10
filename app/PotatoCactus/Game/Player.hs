module PotatoCactus.Game.Player where

import qualified PotatoCactus.Game.Movement.MovementEntity as M (MovementEntity, issueWalkCommand)
import PotatoCactus.Game.Movement.PositionXY (PositionXY)
import PotatoCactus.Game.Movement.WalkingStep (WalkingStep)
import PotatoCactus.Game.Position (GetPosition (getPosition))
import PotatoCactus.Game.Typing (Advance (advance))

data Player = Player
  { username :: String,
    movement :: M.MovementEntity
  }
  deriving (Show)

-- e = Player {username = "foo", movement = StaticMovement {position_ = }}

instance GetPosition Player where
  getPosition = getPosition . movement

instance Advance Player where
  advance p = p {movement = advance (movement p)}

issueWalkCommand :: (PositionXY, Bool, [WalkingStep]) -> Player -> Player
issueWalkCommand (startPos, isRunning, steps) p =
  p {movement = M.issueWalkCommand (movement p) startPos steps}
