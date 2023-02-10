module PotatoCactus.Game.Movement.PlayerWalkMovement where

import PotatoCactus.Game.Movement.Direction (Direction (None), directionBetween)
import PotatoCactus.Game.Movement.PositionXY (PositionXY (PositionXY), toXY)
import PotatoCactus.Game.Movement.WalkingStep (WalkingStep, toPosition)
import PotatoCactus.Game.Position (GetPosition, Position (x), getPosition, y)
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

queueWalk :: PlayerWalkMovement -> PositionXY -> [WalkingStep] -> PlayerWalkMovement
queueWalk current (PositionXY startX startY) steps =
  let start = (position_ current) {x = startX, y = startY}
   in current {queue_ = start : map (toPosition start) steps}

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

mockQueue_ :: Position -> [Position]
mockQueue_ pos =
  [ pos {x = (x pos) + 1},
    pos {x = (x pos) + 2},
    pos {x = (x pos) + 3},
    pos {x = (x pos) + 4},
    pos {x = (x pos) + 5},
    pos {x = (x pos) + 6},
    pos {x = (x pos) + 7},
    pos {x = (x pos) + 8},
    pos {x = (x pos) + 9},
    pos {x = (x pos) + 10},
    pos {x = (x pos) + 11},
    pos {x = (x pos) + 12},
    pos {x = (x pos) + 13},
    pos {x = (x pos) + 14},
    pos {x = (x pos) + 15},
    pos {x = (x pos) + 16},
    pos {x = (x pos) + 17},
    pos {x = (x pos) + 18},
    pos {x = (x pos) + 19},
    pos {x = (x pos) + 20}
  ]
