module PotatoCactus.Game.Scripting.ScriptUpdates where

import Data.Aeson (Value)
import PotatoCactus.Game.Combat.CombatEntity (CombatTarget)
import PotatoCactus.Game.Combat.Hit (Hit)
import PotatoCactus.Game.Definitions.ItemDefinitions (ItemId)
import PotatoCactus.Game.Entity.Animation.Animation (Animation)
import PotatoCactus.Game.Entity.GroundItem.GroundItem (GroundItem (GroundItem))
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction)
import PotatoCactus.Game.Entity.Npc.Npc (Npc, NpcIndex)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject)
import PotatoCactus.Game.Player (Player, PlayerIndex)
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Game.Scripting.Actions.CreateInterface (CreateInterfaceRequest, WidgetId)
import PotatoCactus.Game.Scripting.Actions.ScriptInvocation (ScriptInvocation)
import PotatoCactus.Game.Scripting.Actions.SpawnNpcRequest (SpawnNpcRequest)

data GameEvent
  = ServerInitEvent
  | PlayerInteractionEvent Player Interaction -- maps to NpcInteractionEvent, ObjectInteractionEvent, ItemOnObjectInteractionEvent and NpcAttackInteractionEvent
  | PlayerAttackEvent Player CombatTarget
  | NpcAttackEvent Npc CombatTarget
  | NpcCannotReachTargetEvent Npc CombatTarget
  | NpcDeadEvent Npc
  | NpcEntityTickEvent Npc
  | PlayerCommandEvent PlayerIndex String [String]
  | ScriptInvokedEvent ScriptInvocation
  | DropItemEvent PlayerIndex WidgetId ItemId Int
  deriving (Show)

data ScriptActionResult
  = AddGameObject DynamicObject
  | ClearPlayerInteraction PlayerIndex
  | DispatchAttackPlayerToNpc PlayerIndex NpcIndex Hit
  | DispatchAttackNpcToPlayer NpcIndex PlayerIndex Hit
  | NpcSetAnimation NpcIndex Animation
  | NpcQueueWalk NpcIndex Position
  | NpcMoveTowardsTarget Npc
  | NpcSetForcedChat NpcIndex String
  | SpawnNpc SpawnNpcRequest
  | SendMessage PlayerIndex String
  | SetPlayerPosition PlayerIndex Position
  | SetPlayerAnimation PlayerIndex Animation
  | InternalRemoveNpcTargetReferences NpcIndex
  | InternalProcessingComplete -- Sentinel token to indicate script execution complete
  | InvokeScript ScriptInvocation Int
  | CreateInterface PlayerIndex CreateInterfaceRequest
  | ClearStandardInterface PlayerIndex
  | GiveItem PlayerIndex ItemId Int
  | SubtractItem PlayerIndex ItemId Int -- Remove quantity of item, from anywhere in the inventory
  | RemoveItemStack PlayerIndex ItemId Int -- Remove stack of item at index
  | SpawnGroundItem GroundItem
  | SetPlayerEntityData PlayerIndex String Value
  | InternalNoop
  | ServerPrintMessage String -- for testing
  deriving (Show)
