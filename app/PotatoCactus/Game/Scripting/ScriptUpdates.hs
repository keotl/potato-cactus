module PotatoCactus.Game.Scripting.ScriptUpdates where

import PotatoCactus.Game.Combat.CombatEntity (CombatTarget)
import PotatoCactus.Game.Combat.Hit (Hit)
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction)
import PotatoCactus.Game.Entity.Npc.Npc (Npc, NpcIndex)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject)
import PotatoCactus.Game.Player (Player, PlayerIndex)

data GameEvent
  = PlayerInteractionEvent Player Interaction
  | PlayerAttackEvent Player CombatTarget
  | NpcAttackEvent Npc CombatTarget
  | NpcEntityTickEvent Npc

data ScriptActionResult
  = AddGameObject DynamicObject
  | ClearPlayerInteraction PlayerIndex
  | DispatchAttackPlayerToNpc PlayerIndex NpcIndex Hit
  | DispatchAttackNpcToPlayer NpcIndex PlayerIndex Hit
  | UpdateNpc NpcIndex Npc
