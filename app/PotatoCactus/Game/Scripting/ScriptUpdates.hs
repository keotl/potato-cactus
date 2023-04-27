module PotatoCactus.Game.Scripting.ScriptUpdates where

import PotatoCactus.Game.Combat.CombatEntity (CombatTarget)
import PotatoCactus.Game.Combat.Hit (Hit)
import PotatoCactus.Game.Entity.Animation.Animation (Animation)
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction)
import PotatoCactus.Game.Entity.Npc.Npc (Npc, NpcIndex)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject)
import PotatoCactus.Game.Player (Player, PlayerIndex)
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Game.Scripting.Actions.SpawnNpcRequest (SpawnNpcRequest)

data GameEvent
  = ServerInitEvent
  | PlayerInteractionEvent Player Interaction -- maps to NpcInteractionEvent, ObjectInteractionEvent and NpcAttackInteractionEvent
  | PlayerAttackEvent Player CombatTarget
  | NpcAttackEvent Npc CombatTarget
  | NpcCannotReachTargetEvent Npc CombatTarget
  | NpcDeadEvent Npc
  | NpcEntityTickEvent Npc

data ScriptActionResult
  = AddGameObject DynamicObject
  | ClearPlayerInteraction PlayerIndex
  | DispatchAttackPlayerToNpc PlayerIndex NpcIndex Hit
  | DispatchAttackNpcToPlayer NpcIndex PlayerIndex Hit
  | NpcSetAnimation NpcIndex Animation
  | NpcQueueWalk NpcIndex Position
  | NpcMoveTowardsTarget Npc
  | SpawnNpc SpawnNpcRequest
  | InternalRemoveNpcTargetReferences NpcIndex
  | InternalProcessingComplete -- Sentinel token to indicate script execution complete
  | InternalNoop
  | DummyEvent String -- for testing
  | UpdateNpc NpcIndex Npc -- deprecated
  deriving (Show)
