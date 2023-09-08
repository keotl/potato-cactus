module Game.Player.ClosestInteractableTileCalcTests where

import PotatoCactus.Game.Entity.Interaction.ClosestInteractableTileCalc (selectClosestInteractableTile)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Position (Position (Position, x, y))
import Test.HUnit

closestInteractableTileCalcTests :: Test
closestInteractableTileCalcTests =
  TestList
    [ TestCase
        ( assertEqual
            "finds tile on border around object"
            (targetPos {x = 102})
            ( selectClosestInteractableTile
                (2, 2)
                targetPos
                (targetPos {x = 105})
            )
        ),
      TestCase
        ( assertEqual
            "does not suggest position on corner of allowed boundary region"
            (targetPos {x = 100, y = 99})
            ( selectClosestInteractableTile
                (1, 1)
                targetPos
                (targetPos {x = 99, y = 99})
            )
        ),
      TestCase
        ( assertEqual
            "suggests position outside the entity when the actor is considered inside"
            (targetPos {x = 99, y = 100})
            ( selectClosestInteractableTile
                (1, 1)
                targetPos
                targetPos
            )
        )
    ]

targetPos :: Position
targetPos = Position 100 100 0
