module PotatoCactus.Game.Movement.PlayerMovement where

import PotatoCactus.Game.Movement.Direction (Direction (None), directionBetween)
import PotatoCactus.Game.Movement.InterpolatePath (interpolatePath)
import PotatoCactus.Game.Movement.PositionXY (PositionXY (PositionXY), toXY)
import PotatoCactus.Game.Movement.WalkingStep (WalkingStep, toPosition)
import PotatoCactus.Game.Position (GetPosition, Position (x), chunkX, chunkY, getPosition, y)
import PotatoCactus.Game.Typing (Advance (advance))
import PotatoCactus.Utils.Flow ((|>))

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
    m
      { walkingDirection = None,
        runningDirection = None,
        isTeleporting = False,
        hasChangedRegion = False
      }
      |> doStep_ (isRunning m)
      |> doStep_ (isRunning m)

doStep_ :: Bool -> PlayerMovement -> PlayerMovement
doStep_ False m =
  case (walkingDirection m, queue_ m) of
    (None, x : xs) ->
      m
        { position_ = x,
          queue_ = xs,
          walkingDirection = directionBetween (toXY (position_ m)) (toXY x),
          hasChangedRegion = hasChangedRegion m || shouldUpdateRegion_ (lastRegionUpdate_ m) x,
          lastRegionUpdate_ = if shouldUpdateRegion_ (lastRegionUpdate_ m) x then x else lastRegionUpdate_ m
        }
    _ -> m
doStep_ True m =
  case (walkingDirection m, runningDirection m, queue_ m) of
    (None, _, _) -> m |> doStep_ False
    (_, None, x : xs) ->
      m
        { position_ = x,
          queue_ = xs,
          runningDirection = directionBetween (toXY (position_ m)) (toXY x),
          hasChangedRegion = hasChangedRegion m || shouldUpdateRegion_ (lastRegionUpdate_ m) x,
          lastRegionUpdate_ = if shouldUpdateRegion_ (lastRegionUpdate_ m) x then x else lastRegionUpdate_ m
        }
    _ -> m

queueWalk :: PlayerMovement -> PositionXY -> [WalkingStep] -> PlayerMovement
queueWalk current (PositionXY startX startY) steps =
  let firstStep = (position_ current) {x = startX, y = startY}
   in current
        { queue_ =
            interpolatePath
              (position_ current : firstStep : map (toPosition firstStep) steps)
        }

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
immediatelyQueueMovement current path =
  if isValidPath_ (position_ current : path)
    then
      current {queue_ = path}
        |> (\m -> doStep_ (isRunning m) m)
        |> (\m -> doStep_ (isRunning m) m)
    else current

isValidPath_ :: [Position] -> Bool
isValidPath_ [] = True
isValidPath_ [pos] = True
isValidPath_ (firstPos : otherPos) =
  let secondPos = head otherPos
   in let dx = (x secondPos - x firstPos)
       in let dy = (y secondPos - y firstPos)
           in abs dx <= 1 && abs dy <= 1

