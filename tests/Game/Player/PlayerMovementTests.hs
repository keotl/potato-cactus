module Game.Player.PlayerMovementTests where

import qualified PotatoCactus.Game.Movement.Direction as Direction
import PotatoCactus.Game.Movement.PlayerMovement (PlayerMovement (..), create, immediatelyQueueMovement, queueWalk, setRunning)
import PotatoCactus.Game.Movement.PositionXY (PositionXY (PositionXY))
import PotatoCactus.Game.Movement.WalkingStep (WalkingStep (WalkingStep))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position), x)
import PotatoCactus.Game.Typing (Advance (advance))
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

playerMovementTests :: Test
playerMovementTests =
  TestList
    [ TestCase
        ( assertEqual
            "advance a single step when walking"
            pos {x = 101}
            ( movement {isRunning = False}
                |> (\m -> queueWalk m (PositionXY 101 100) [WalkingStep 1 0])
                |> advance
                |> getPosition
            )
        ),
      TestCase
        ( assertEqual
            "advance two steps when running"
            pos {x = 102}
            ( movement {isRunning = True}
                |> (\m -> queueWalk m (PositionXY 101 100) [WalkingStep 1 0])
                |> advance
                |> getPosition
            )
        ),
      TestCase
        ( assertEqual
            "sets walkingDirection to direction of first step"
            Direction.East
            ( movement {isRunning = True}
                |> (\m -> queueWalk m (PositionXY 101 100) [WalkingStep 0 1])
                |> advance
                |> walkingDirection
            )
        ),
      TestCase
        ( assertEqual
            "sets runningDirection to direction to second step"
            Direction.North
            ( movement {isRunning = True}
                |> (\m -> queueWalk m (PositionXY 101 100) [WalkingStep 0 1])
                |> advance
                |> runningDirection
            )
        ),
      TestCase
        ( assertEqual
            "immediatelyQueueMovement does not advance if player has already moved this tick"
            pos {x = 102}
            ( movement {isRunning = True}
                |> (\m -> queueWalk m (PositionXY 101 100) [WalkingStep 1 0])
                |> advance
                |> flip immediatelyQueueMovement [pos {x = 103}]
                |> getPosition
            )
        ),
      TestCase
        ( assertEqual
            "immediatelyQueueMovement adds a step while running if has made only a single step this tick"
            pos {x = 102}
            ( movement {isRunning = True}
                |> (\m -> queueWalk m (PositionXY 101 100) [])
                |> advance
                |> flip immediatelyQueueMovement [pos {x = 102}]
                |> getPosition
            )
        ),
      TestCase
        ( assertEqual
            "immediatelyQueueMovement adds a step immediately while walking if has not made any steps this tick"
            pos {x = 101}
            ( movement {isRunning = False}
                |> advance
                |> flip immediatelyQueueMovement [pos {x = 101}]
                |> getPosition
            )
        ),
      TestCase
        ( assertEqual
            "immediatelyQueueMovement does not move immediately if the player has already walked this tick"
            pos {x = 101}
            ( movement {isRunning = False}
                |> (\m -> queueWalk m (PositionXY 101 100) [])
                |> advance
                |> flip immediatelyQueueMovement [pos {x = 102}]
                |> getPosition
            )
        ),
      TestCase
        ( assertEqual
            "immediatelyQueueMovement rejects non-adjacent start path"
            (pos, [])
            ( movement
                |> flip immediatelyQueueMovement [pos {x = 200}]
                |> \m -> (getPosition m, queue_ m)
            )
        ),
      TestCase
        ( assertEqual
            "queueWalk corrects walking steps with a dx or dy larger than 1"
            (pos {x = 101})
            ( movement
                |> (\m -> queueWalk m (PositionXY 100 100) [WalkingStep 10 0])
                |> advance
                |> getPosition
            )
        )
    ]

pos :: Position
pos = Position 100 100 0

movement :: PlayerMovement
movement = create pos
