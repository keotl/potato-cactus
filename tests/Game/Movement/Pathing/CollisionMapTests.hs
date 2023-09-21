module Game.Movement.Pathing.CollisionMapTests where

import PotatoCactus.Game.Movement.Pathing.CollisionMap (CollisionMap, create)
import Test.HUnit

collisionMapTests :: Test
collisionMapTests =
  TestList
    [ TestCase
        ( assertEqual
            "marks solid object"
            1
            ( collisionMap
                |> markSolidOccupant (pos, (1, 1), 0)
            )
        )
    ]

pos :: Position
pos = Position 100 100 0

collisionMap :: CollisionMap
collisionMap = create
