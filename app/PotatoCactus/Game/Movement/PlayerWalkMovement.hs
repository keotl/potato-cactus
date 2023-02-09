module PotatoCactus.Game.Movement.PlayerWalkMovement where

import PotatoCactus.Game.Movement.Direction (Direction (None), directionBetween)
import PotatoCactus.Game.Movement.PositionXY (toXY)
import PotatoCactus.Game.Position (GetPosition, Position, getPosition)
import PotatoCactus.Game.Typing (Advance (advance))

data PlayerWalkMovement = PlayerWalkMovement
  { position_ :: Position,
    queue_ :: [Position],
    isTeleporting :: Bool,
    isRunning :: Bool,
    runEnergy :: Int,
    walkingDirection :: Direction,
    runningDirection :: Direction,
    skipUpdate_ :: Bool -- to make sure that a new player does not immediately transitions to isTeleporting = False
  }
  deriving (Show)

instance GetPosition PlayerWalkMovement where
  getPosition = position_

-- Running does two steps
instance Advance PlayerWalkMovement where
  advance m =
    case (skipUpdate_ m, queue_ m, isRunning m) of
      (True, _, _) -> m {skipUpdate_ = False}
      (_, [], _) -> m {walkingDirection = None, runningDirection = None, isTeleporting = False}
      (_, x : xs, False) ->
        m
          { position_ = x,
            queue_ = xs,
            isTeleporting = False,
            walkingDirection = directionBetween (toXY (position_ m)) (toXY x),
            runningDirection = None
          }
      (_, x : xs, True) -> m {position_ = x, queue_ = xs} -- TODO - Implement running  - keotl 2023-02-09

create :: Position -> PlayerWalkMovement
create pos =
  PlayerWalkMovement
    { position_ = pos,
      queue_ = [],
      isRunning = False,
      isTeleporting = True,
      runEnergy = 100,
      walkingDirection = None,
      runningDirection = None,
      skipUpdate_ = True
    }
