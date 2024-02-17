module Game.Entity.Npc.NpcMovementTests where

import PotatoCactus.Game.Entity.Npc.NpcMovement (NpcMovement, create, immediatelyQueueMovement)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (..))
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

npcMovementTests :: Test
npcMovementTests =
  TestList
    [ TestCase
        ( assertEqual
            "immediatelyQueueMovement queues for next tick if has already moved this tick"
            destination
            ( immediatelyQueueMovement movement [destination]
                |> flip immediatelyQueueMovement [destination {x = 102}]
                |> getPosition
            )
        )
    ]

movement :: NpcMovement
movement = create (Position 100 100 0)

destination :: Position
destination = Position 101 100 0
