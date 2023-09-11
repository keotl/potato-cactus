module PotatoCactus.Game.Combat.AdvanceCombatEntityDeps where

import PotatoCactus.Game.Combat.CombatEntity (CombatTarget, CombatTargetStatus)

data AdvanceCombatEntityDeps = AdvanceCombatEntityDeps
  { locateTarget :: CombatTarget -> CombatTargetStatus
  }
