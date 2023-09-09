module PotatoCactus.Game.Scripting.Events.NpcEvents (createNpcEvents) where

import Data.Maybe (catMaybes)
import PotatoCactus.Game.Combat.CombatEntity (CombatTarget (None), cooldown)
import qualified PotatoCactus.Game.Combat.CombatEntity as Combat
import PotatoCactus.Game.Entity.Npc.Npc (Npc (canReachTarget, combat))
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (InternalNpcCannotReachTargetEvent, NpcAttackEvent, NpcDeadEvent, NpcEntityTickEvent))
import PotatoCactus.Utils.Flow ((|>))

createNpcEvents :: Npc -> [GameEvent]
createNpcEvents npc =
  NpcEntityTickEvent npc :
  catMaybes
    [ attackEvent_ npc,
      cannotReachEvent_ npc,
      npcDeadEvent_ npc
    ]

attackEvent_ :: Npc -> Maybe GameEvent
attackEvent_ npc =
  case (Combat.target . combat $ npc, canReachTarget npc) of
    (None, _) -> Nothing
    (target, True) ->
      if 0 == (cooldown . combat $ npc)
        then Just $ NpcAttackEvent npc target
        else Nothing
    (_, _) -> Nothing

cannotReachEvent_ :: Npc -> Maybe GameEvent
cannotReachEvent_ npc =
  if canReachTarget npc
    then Nothing
    else Just $ InternalNpcCannotReachTargetEvent npc (Combat.target . combat $ npc)

npcDeadEvent_ :: Npc -> Maybe GameEvent
npcDeadEvent_ npc =
  case npc |> combat |> Combat.state of
    Combat.Dying -> Just $ NpcDeadEvent npc
    _ -> Nothing
