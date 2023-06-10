module Game.GroundItemsUpdateDiffTests where

import PotatoCactus.Client.GroundItemsUpdate.GroundItemsUpdateDiff (GroundItemClientView (GroundItemClientView), GroundItemDiff (Added, Removed, Retained), computeDiff)
import PotatoCactus.Game.Position (Position (Position))
import Test.HUnit

groundItemsUpdateDiffTests :: Test
groundItemsUpdateDiffTests =
  TestList
    [ TestCase
        ( assertEqual
            "added item"
            [Added item]
            (computeDiff [] [item])
        ),
      TestCase
        ( assertEqual
            "removed item"
            [Removed item]
            (computeDiff [item] [])
        ),
      TestCase
        ( assertEqual
            "retained item"
            [Retained item]
            (computeDiff [item] [item])
        )
    ]

item :: GroundItemClientView
item = GroundItemClientView 123 1 (Position 0 0 0)
