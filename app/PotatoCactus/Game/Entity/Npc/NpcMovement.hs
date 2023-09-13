module PotatoCactus.Game.Entity.Npc.NpcMovement where

import PotatoCactus.Game.Movement.Direction (Direction (None, North), directionBetween)
import PotatoCactus.Game.Movement.PositionXY (toXY)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position)
import PotatoCactus.Game.Typing (Advance (advance))

data NpcMovement = NpcMovement
  { position_ :: Position,
    queue_ :: [Position],
    walkingDirection :: Direction
  }
  deriving (Show)

instance GetPosition NpcMovement where
  getPosition = position_

instance Advance NpcMovement where
  advance m =
    case queue_ m of
      [] -> m {walkingDirection = None}
      (x : xs) ->
        m
          { position_ = x,
            queue_ = xs,
            walkingDirection = directionBetween (toXY (position_ m)) (toXY x)
          }

create :: Position -> NpcMovement
create pos =
  NpcMovement
    { position_ = pos,
      queue_ = [],
      walkingDirection = None
    }

-- for scripts, start pathing immediately if has not already moved on this tick
immediatelyQueueMovement :: NpcMovement -> [Position] -> NpcMovement
immediatelyQueueMovement m path =
  case walkingDirection m of
    None ->  advance m {queue_ = path}
    _ -> m { queue_ = path}
