module Game.Movement.Pathing.PathPlannerTests where

import PotatoCactus.Game.Movement.Pathing.CollisionMap (CollisionMap, markSolidOccupant)
import PotatoCactus.Game.Movement.Pathing.PathPlanner (findPathNaive)
import qualified PotatoCactus.Game.Movement.Pathing.TileFlagsMap as TileFlagsMap
import PotatoCactus.Game.Position
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

pathPlannerTests :: Test
pathPlannerTests =
  TestList
    [ TestCase
        ( assertEqual
            "findPathNaive does not path around object if directly opposite"
            --   x x
            -- @ x x @
            --
            []
            ( findPathNaive collisionMap (Position 99 100 0) (Position 102 100 0)
            )
        ),
      TestCase
        ( assertEqual
            "findPathNaive glides along side of object"
            --   x x
            --   x x @
            --   @ . .
            [Position 101 99 0, Position 102 99 0, Position 102 100 0]
            ( findPathNaive collisionMap (Position 100 99 0) (Position 102 100 0)
            )
        )
    ]

collisionMap :: CollisionMap
collisionMap =
  TileFlagsMap.create
    |> markSolidOccupant (Position 100 100 0, (2, 2), 0)
