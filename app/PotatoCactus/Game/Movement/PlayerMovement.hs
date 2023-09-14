module PotatoCactus.Game.Movement.PlayerMovement where

import PotatoCactus.Game.Movement.Direction (Direction (None), directionBetween)
import PotatoCactus.Game.Movement.InterpolatePath (interpolatePath)
import PotatoCactus.Game.Movement.PositionXY (PositionXY (PositionXY), toXY)
import PotatoCactus.Game.Movement.WalkingStep (WalkingStep, toPosition)
import PotatoCactus.Game.Position (GetPosition, Position (x), chunkX, chunkY, getPosition, y)
import PotatoCactus.Game.Typing (Advance (advance))

data PlayerMovement = PlayerMovement
  { position_ :: Position,
    queue_ :: [Position],
    isTeleporting :: Bool,
    isRunning :: Bool,
    runEnergy :: Int,
    walkingDirection :: Direction,
    runningDirection :: Direction,
    hasChangedRegion :: Bool,
    lastRegionUpdate_ :: Position
  }
  deriving (Show)

instance GetPosition PlayerMovement where
  getPosition = position_

-- Running does two steps
instance Advance PlayerMovement where
  advance m =
    case (queue_ m, isRunning m) of
      ([], _) -> m {walkingDirection = None, runningDirection = None, isTeleporting = False, hasChangedRegion = False}
      (x : xs, False) ->
        m
          { position_ = x,
            queue_ = xs,
            isTeleporting = False,
            walkingDirection = directionBetween (toXY (position_ m)) (toXY x),
            runningDirection = None,
            hasChangedRegion = shouldUpdateRegion_ (lastRegionUpdate_ m) x,
            lastRegionUpdate_ = if shouldUpdateRegion_ (lastRegionUpdate_ m) x then x else lastRegionUpdate_ m
          }
      (x : xs, True) ->
        m
          { position_ = case xs of
              [] -> x
              (y : ys) -> y,
            queue_ = case xs of
              [] -> []
              (y : ys) -> ys,
            isTeleporting = False,
            walkingDirection = directionBetween (toXY (position_ m)) (toXY x),
            runningDirection = case xs of
              [] -> None
              (y : ys) -> directionBetween (toXY x) (toXY y),
            hasChangedRegion = shouldUpdateRegion_ (lastRegionUpdate_ m) x,
            lastRegionUpdate_ = if shouldUpdateRegion_ (lastRegionUpdate_ m) x then x else lastRegionUpdate_ m
          }

queueWalk :: PlayerMovement -> PositionXY -> [WalkingStep] -> PlayerMovement
queueWalk current (PositionXY startX startY) steps =
  let start = (position_ current) {x = startX, y = startY}
   in current {queue_ = interpolatePath (position_ current : start : map (toPosition start) steps)}

create :: Position -> PlayerMovement
create pos =
  PlayerMovement
    { position_ = pos,
      queue_ = [],
      isRunning = True,
      isTeleporting = True,
      runEnergy = 100,
      hasChangedRegion = True,
      walkingDirection = None,
      runningDirection = None,
      lastRegionUpdate_ = pos
    }

shouldUpdateRegion_ :: Position -> Position -> Bool
shouldUpdateRegion_ lastUpdate currentPos =
  let (deltaX, deltaY) = (x currentPos - chunkX lastUpdate * 8, y currentPos - chunkY lastUpdate * 8)
   in deltaX < 16 || deltaX >= 88 || deltaY < 16 || deltaY > 88

isStopped :: PlayerMovement -> Bool
isStopped = null . queue_

setPosition :: PlayerMovement -> Position -> PlayerMovement
setPosition m pos =
  m {position_ = pos, isTeleporting = True, hasChangedRegion = True, lastRegionUpdate_ = pos}


setRunning :: PlayerMovement -> Bool -> PlayerMovement
setRunning m running =
  m {isRunning = running}

-- for scripts, set new position instantly
immediatelyQueueMovement :: PlayerMovement -> [Position] -> PlayerMovement
immediatelyQueueMovement m path =
  m {queue_ = path}
