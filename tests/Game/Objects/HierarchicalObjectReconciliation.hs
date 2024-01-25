module Game.Objects.HierarchicalObjectReconciliation where

import qualified PotatoCactus.Game.Entity.Object.DynamicObject as DynamicObject
import qualified PotatoCactus.Game.Entity.Object.DynamicObject as Visible
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Entity.Object.HierarchicalObjectReconciliation (reconcileObjects)
import PotatoCactus.Game.Position (Position (Position))
import Test.HUnit

hierarchicalObjectReconciliationTests :: Test
hierarchicalObjectReconciliationTests =
  TestList
    [ TestCase
        ( assertEqual
            "adds static objects"
            [staticObj]
            (reconcileObjects [] [staticObj])
        ),
      TestCase
        ( assertEqual
            "adds added dynamic objects"
            [obj]
            (reconcileObjects [DynamicObject.Added obj] [])
        ),
      TestCase
        ( assertEqual
            "selects the dynamic object when replacing a static one"
            [obj]
            (reconcileObjects [DynamicObject.Replacing obj staticObj] [staticObj])
        ),
      TestCase
        ( assertEqual
            "hides a static object when removed in the static set"
            []
            (reconcileObjects [DynamicObject.Removed staticObj] [staticObj])
        )
    ]

pos :: Position
pos = Position 0 0 0

obj :: GameObject
obj = GameObject 123 pos 0 0

staticObj :: GameObject
staticObj = GameObject 456 pos 0 0
