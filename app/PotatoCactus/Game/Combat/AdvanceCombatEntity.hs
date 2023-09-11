module PotatoCactus.Game.Combat.AdvanceCombatEntity (advanceCombatEntity) where

import PotatoCactus.Game.Combat.AdvanceCombatEntityDeps (AdvanceCombatEntityDeps (..))
import PotatoCactus.Game.Combat.CombatEntity (CombatAction (AttackTarget, MoveTowardsTarget), CombatEntity (..), CombatState (..), CombatTarget (..), CombatTargetStatus (..))
import PotatoCactus.Utils.Flow ((|>))

advanceCombatEntity :: AdvanceCombatEntityDeps -> CombatEntity -> CombatEntity
advanceCombatEntity deps c =
  case state c of
    Dying -> c {state = Dead, pendingActions = []}
    Dead -> c
    Alive ->
      c
        { hits = [],
          cooldown = max 0 (cooldown c - 1),
          state = if hitpoints c == 0 then Dying else Alive,
          pendingActions = []
        }
        |> updateTargeting_ deps

updateTargeting_ :: AdvanceCombatEntityDeps -> CombatEntity -> CombatEntity
updateTargeting_ deps c =
  case locateTarget deps (target c) of
    ShouldDisengage -> c {target = None}
    ShouldPathTo -> c {pendingActions = [MoveTowardsTarget]}
    InRange -> c {pendingActions = [AttackTarget | cooldown c == 0]}
