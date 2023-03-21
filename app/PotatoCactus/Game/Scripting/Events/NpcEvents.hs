module PotatoCactus.Game.Scripting.Events.NpcEvents (createNpcEvents) where

import Data.Maybe (catMaybes)
import PotatoCactus.Game.Combat.CombatEntity (CombatTarget (None), cooldown)
import qualified PotatoCactus.Game.Combat.CombatEntity as Combat
import PotatoCactus.Game.Entity.Npc.Npc (Npc (combat))
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (NpcAttackEvent, NpcEntityTickEvent))

createNpcEvents :: Npc -> [GameEvent]
createNpcEvents npc =
  NpcEntityTickEvent npc : catMaybes [attackEvent_ npc]

attackEvent_ :: Npc -> Maybe GameEvent
attackEvent_ npc =
  case Combat.target . combat $ npc of
    None -> Nothing
    target ->
      if 0 == (cooldown . combat $ npc)
      -- TODO - check can reach  - keotl 2023-03-20
        then Just $ NpcAttackEvent npc target
        else Nothing
