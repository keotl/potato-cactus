module PotatoCactus.Game.Scripting.ScriptUpdates where

import Data.Aeson (Value)
import Data.Binary (Word32, Word8)
import PotatoCactus.Game.Combat.CombatEntity (CombatTarget)
import PotatoCactus.Game.Combat.Hit (Hit)
import PotatoCactus.Game.Definitions.Types.ItemDefinition (ItemId)
import PotatoCactus.Game.Entity.Animation.Animation (Animation)
import PotatoCactus.Game.Entity.GroundItem.GroundItem (GroundItem (GroundItem))
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction)
import PotatoCactus.Game.Entity.Interaction.Target (InteractionTarget)
import PotatoCactus.Game.Entity.Npc.Npc (Npc, NpcIndex)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject, GameObjectType)
import PotatoCactus.Game.Player (Player, PlayerIndex)
import PotatoCactus.Game.PlayerUpdate.VarpSet (VarpId)
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Game.Scripting.Actions.CreateInterface (CreateInterfaceRequest, WidgetId)
import PotatoCactus.Game.Scripting.Actions.ScriptInvocation (ScriptInvocation)
import PotatoCactus.Game.Scripting.Actions.SpawnNpcRequest (SpawnNpcRequest)

data GameEvent
  = ServerInitEvent
  | PlayerInteractionEvent Player Interaction -- maps to NpcInteractionEvent, ObjectInteractionEvent, ItemOnObjectInteractionEvent, NpcAttackInteractionEvent and PickupItemInteractionEvent
  | InternalPlayerInteractionPendingPathingEvent Player InteractionTarget
  | PlayerAttackEvent Player CombatTarget
  | NpcAttackEvent Npc CombatTarget
  | InternalNpcCannotReachCombatTargetEvent Npc Position
  | InternalPlayerCannotReachCombatTargetEvent Player Position
  | NpcDeadEvent Npc
  | NpcEntityTickEvent Npc
  | PlayerCommandEvent PlayerIndex String [String]
  | ScriptInvokedEvent ScriptInvocation
  | DropItemEvent PlayerIndex WidgetId ItemId Int
  deriving (Show)

data ScriptActionResult
  = SpawnGameObject GameObject
  | RemoveGameObject (Position, GameObjectType)
  | ClearPlayerInteraction PlayerIndex
  | DispatchAttackPlayerToNpc PlayerIndex NpcIndex Hit
  | DispatchAttackNpcToPlayer NpcIndex PlayerIndex Hit
  | NpcSetAnimation NpcIndex Animation
  | NpcQueueWalk NpcIndex Position
  | -- | InternalNpcMoveTowardsCombatTarget NpcIndex
    NpcSetForcedChat NpcIndex String
  | SpawnNpc SpawnNpcRequest
  | SendMessage PlayerIndex String
  | SetPlayerPosition PlayerIndex Position
  | SetPlayerAnimation PlayerIndex Animation
  | InternalProcessingComplete -- Sentinel token to indicate script execution complete
  | InvokeScript ScriptInvocation Int
  | CreateInterface PlayerIndex CreateInterfaceRequest
  | ClearStandardInterface PlayerIndex
  | GiveItem PlayerIndex ItemId Int
  | SubtractItem PlayerIndex ItemId Int -- Remove quantity of item, from anywhere in the inventory
  | RemoveItemStack PlayerIndex ItemId Int -- Remove stack of item at index
  | SpawnGroundItem GroundItem
  | RemoveGroundItem ItemId Int Position (Maybe PlayerIndex)
  | SetPlayerEntityData PlayerIndex String Value
  | SetPlayerVarp PlayerIndex (VarpId, Word32)
  | SetPlayerVarbit PlayerIndex (VarpId, Word8, Word8, Word32)
  | InternalNoop
  | ServerPrintMessage String -- for testing
  | InternalSetPlayerInteractionPending PlayerIndex
  | PlayerQueueWalk PlayerIndex Position
  | InternalPlayerQueueWalkPath PlayerIndex [Position]
  deriving (Show)
