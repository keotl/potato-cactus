module Game.InterpolatePathTests where

import GHC.Float (expts10)
import PotatoCactus.Game.Movement.InterpolatePath (interpolatePath)
import PotatoCactus.Game.Position (Position (Position))
import Test.HUnit

interpolatePathTests :: Test
interpolatePathTests =
  TestList
    [ TestCase
        ( assertEqual
            "simple path"
            [Position 1 0 0, Position 2 0 0]
            ( interpolatePath [Position 0 0 0, Position 2 0 0]
            )
        ),
      TestCase
        ( assertEqual
            "complex path"
            [Position 1 0 0, Position 2 0 0, Position 2 1 0, Position 2 2 0]
            ( interpolatePath [Position 0 0 0, Position 2 0 0, Position 2 2 0]
            )
        )
    ]
