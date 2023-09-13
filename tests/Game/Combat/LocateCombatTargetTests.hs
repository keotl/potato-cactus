module Game.Combat.LocateCombatTargetTests where

import PotatoCactus.Game.Combat.AdvanceCombatEntityDeps (AdvanceCombatEntityDeps (AdvanceCombatEntityDeps))
import PotatoCactus.Game.Combat.CombatEntity (CombatTarget (NpcTarget), CombatTargetStatus (InRange, ShouldDisengage, ShouldPathTo))
import PotatoCactus.Game.Combat.LocateCombatTarget (LocateTargetArgs (LocateTargetArgs), locateCombatTarget)
import PotatoCactus.Game.Position
import Test.HUnit

locateCombatTargetTests :: Test
locateCombatTargetTests =
  TestList
    [ TestCase
        ( assertEqual
            "disengages target further than deAggroRange"
            ShouldDisengage
            (locateCombatTarget deps args targetPos {x = 200} target)
        ),
      TestCase
        ( assertEqual
            "requires pathing if the actor is between attack range and aggression range"
            (ShouldPathTo targetPos {x = 101})
            (locateCombatTarget deps args targetPos {x = 105} target)
        ),
      TestCase
        ( assertEqual
            "requires pathing if the actor is under the target"
            (ShouldPathTo targetPos {x = 99})
            (locateCombatTarget deps args targetPos target)
        ),
      TestCase
        ( assertEqual
            "InRange if the target is less or equal to attackRange"
            InRange
            (locateCombatTarget deps args targetPos {x = 101} target)
        )
    ]

targetPos :: Position
targetPos = Position 100 100 0

target :: CombatTarget
target = NpcTarget 1

deps :: AdvanceCombatEntityDeps
deps = AdvanceCombatEntityDeps (\_ -> Just targetPos) (\_ -> Just targetPos) (const (1, 1))

args :: LocateTargetArgs
args = LocateTargetArgs 1 10
