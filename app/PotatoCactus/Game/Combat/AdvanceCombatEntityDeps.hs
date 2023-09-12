module PotatoCactus.Game.Combat.AdvanceCombatEntityDeps where

import PotatoCactus.Game.Combat.CombatEntity (CombatTarget, CombatTargetStatus)
import PotatoCactus.Game.Entity.Npc.Npc (NpcIndex)
import PotatoCactus.Game.Player (PlayerIndex)
import PotatoCactus.Game.Position (Position)

data AdvanceCombatEntityDeps = AdvanceCombatEntityDeps
  { findNpc :: NpcIndex -> Maybe Position,
    findPlayer :: PlayerIndex -> Maybe Position,
    npcSize :: NpcIndex -> (Int, Int)
  }
