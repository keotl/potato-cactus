module Game.Player.CanInteractWithEntityTests where

import PotatoCactus.Game.Entity.Interaction.CanInteractWithEntity (canInteractWithEntity)
import PotatoCactus.Game.Position (Position (..))
import Test.HUnit

canInteractWithEntityTests :: Test
canInteractWithEntityTests =
  TestList
    [ TestCase
        ( assertEqual
            "does not allow interaction when the actor is inside the entity"
            False
            (canInteractWithEntity (2, 2) entityPos entityPos)
        ),
      TestCase
        ( assertEqual
            "does not allow diagonal interaction"
            False
            (canInteractWithEntity (1, 1) entityPos entityPos {x = 99, y = 99})
        )
    ]

entityPos :: Position
entityPos = Position 100 100 0
