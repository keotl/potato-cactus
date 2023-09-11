module Game.Combat.AdvanceCombatEntityTests where

import PotatoCactus.Game.Combat.AdvanceCombatEntity (advanceCombatEntity)
import PotatoCactus.Game.Combat.AdvanceCombatEntityDeps (AdvanceCombatEntityDeps (AdvanceCombatEntityDeps))
import PotatoCactus.Game.Combat.CombatEntity (CombatAction (MoveTowardsTarget), CombatEntity (..), CombatState (Dead, Dying), CombatTarget (None, NpcTarget), CombatTargetStatus (InRange, ShouldDisengage, ShouldPathTo), create, setTarget)
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

advanceCombatEntityTests :: Test
advanceCombatEntityTests =
  TestList
    [ TestCase
        ( assertEqual
            "clears target when disengaging"
            None
            (target $ advanceCombatEntity (deps ShouldDisengage) combatEntity)
        ),
      TestCase
        ( assertEqual
            "queues pathing action when should path to target"
            [MoveTowardsTarget]
            (pendingActions $ advanceCombatEntity (deps ShouldPathTo) combatEntity)
        ),
      TestCase
        ( assertEqual
            "queues attack action when target is in range and cooldown is 0"
            [MoveTowardsTarget]
            (pendingActions $ advanceCombatEntity (deps InRange) combatEntity)
        ),
      TestCase
        ( assertEqual
            "queues nothing when target is in range and cooldown is greater than 0"
            []
            (pendingActions $ advanceCombatEntity (deps InRange) combatEntity {cooldown = 10})
        ),
      TestCase
        ( assertEqual
            "reduces cooldown every tick"
            1
            (cooldown $ advanceCombatEntity anyDeps combatEntity {cooldown = 2})
        ),
      TestCase
        ( assertEqual
            "transitions to Dying when hitpoints reach 0"
            Dying
            (state $ advanceCombatEntity anyDeps combatEntity {hitpoints = 0})
        ),
      TestCase
        ( assertEqual
            "transitions from Dying to Dead on the next tick to allow the death animation"
            Dead
            (state $ advanceCombatEntity anyDeps combatEntity {state = Dying})
        )
    ]

deps :: CombatTargetStatus -> AdvanceCombatEntityDeps
deps desired = AdvanceCombatEntityDeps (const desired)

anyDeps = deps InRange

targetEntity :: CombatTarget
targetEntity = NpcTarget 1

combatEntity :: CombatEntity
combatEntity =
  create 10
    |> flip setTarget targetEntity
