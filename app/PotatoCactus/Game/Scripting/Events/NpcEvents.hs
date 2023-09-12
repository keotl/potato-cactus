module PotatoCactus.Game.Scripting.Events.NpcEvents (createNpcEvents) where

import Data.Maybe (catMaybes)
import PotatoCactus.Game.Combat.CombatEntity (CombatTarget (None), cooldown)
import qualified PotatoCactus.Game.Combat.CombatEntity as Combat
import PotatoCactus.Game.Entity.Npc.Npc (Npc (combat))
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (InternalNpcCannotReachTargetEvent, NpcAttackEvent, NpcDeadEvent, NpcEntityTickEvent))
import PotatoCactus.Utils.Flow ((|>))

createNpcEvents :: Npc -> [GameEvent]
createNpcEvents npc =
  [NpcEntityTickEvent npc]
    ++ catMaybes [npcDeadEvent_ npc]
    ++ combatEvents_ npc

combatEvents_ :: Npc -> [GameEvent]
combatEvents_ npc =
  map (mapCombatAction_ npc) (Combat.pendingActions . combat $ npc)

mapCombatAction_ :: Npc -> Combat.CombatAction -> GameEvent
mapCombatAction_ npc Combat.MoveTowardsTarget = InternalNpcCannotReachTargetEvent npc (Combat.target . combat $ npc)
mapCombatAction_ npc Combat.AttackTarget = NpcAttackEvent npc (Combat.target . combat $ npc)

npcDeadEvent_ :: Npc -> Maybe GameEvent
npcDeadEvent_ npc =
  case npc |> combat |> Combat.state of
    Combat.Dying -> Just $ NpcDeadEvent npc
    _ -> Nothing
