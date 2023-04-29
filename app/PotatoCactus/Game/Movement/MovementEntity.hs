module PotatoCactus.Game.Movement.MovementEntity where

import PotatoCactus.Game.Movement.PlayerWalkMovement (PlayerWalkMovement (isRunning, queue_, shouldUpdateRegion), queueWalk)
import qualified PotatoCactus.Game.Movement.PlayerWalkMovement as PlayerWalkMovement
import PotatoCactus.Game.Movement.PositionXY (PositionXY)
import PotatoCactus.Game.Movement.StaticMovement (StaticMovement)
import qualified PotatoCactus.Game.Movement.StaticMovement as S
import PotatoCactus.Game.Movement.WalkingStep (WalkingStep)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position)
import PotatoCactus.Game.Typing (Advance (advance))

data MovementEntity = PlayerWalkMovement_ PlayerWalkMovement | StaticMovement_ StaticMovement deriving (Show)

instance GetPosition MovementEntity where
  getPosition t = case t of
    PlayerWalkMovement_ m -> getPosition m
    StaticMovement_ m -> getPosition m

instance Advance MovementEntity where
  advance t = case t of
    PlayerWalkMovement_ m -> PlayerWalkMovement_ $ advance m
    StaticMovement_ m -> StaticMovement_ $ advance m

playerWalkMovement :: Position -> MovementEntity
playerWalkMovement = PlayerWalkMovement_ . PlayerWalkMovement.create

issueWalkCommand :: MovementEntity -> PositionXY -> [WalkingStep] -> MovementEntity
issueWalkCommand (PlayerWalkMovement_ m) startPos steps =
  PlayerWalkMovement_ (queueWalk m startPos steps)
issueWalkCommand m _ _ = m

hasChangedRegion :: MovementEntity -> Bool
hasChangedRegion (PlayerWalkMovement_ m) = shouldUpdateRegion m
hasChangedRegion _ = False

setRunning :: MovementEntity -> Bool -> MovementEntity
setRunning (PlayerWalkMovement_ m) running =
  PlayerWalkMovement_ m {isRunning = running}
setRunning m _ = m

isStopped :: MovementEntity -> Bool
isStopped (PlayerWalkMovement_ m) =
  PlayerWalkMovement.isStopped m
isStopped _ = True

-- for scripts, set new position instantly
immediatelyQueueMovement :: MovementEntity -> [Position] -> MovementEntity
immediatelyQueueMovement (PlayerWalkMovement_ m) path =
  PlayerWalkMovement_ $ advance m {queue_ = path}
immediatelyQueueMovement m _ = m

immediatelySetPosition :: MovementEntity -> Position -> MovementEntity
immediatelySetPosition (PlayerWalkMovement_ m) pos =
  PlayerWalkMovement_ $ PlayerWalkMovement.setPosition m pos
immediatelySetPosition (StaticMovement_ m) pos =
  StaticMovement_ $ m {S.position_ = pos}
