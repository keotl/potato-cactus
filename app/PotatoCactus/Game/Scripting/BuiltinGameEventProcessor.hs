module PotatoCactus.Game.Scripting.BuiltinGameEventProcessor where

import Debug.Trace (trace)
import qualified PotatoCactus.Game.Combat.CombatEntity as Combat
import PotatoCactus.Game.Combat.Hit (DamageType (MeleeAttack), Hit (Hit))
import PotatoCactus.Game.Entity.Animation.Animation (Animation (Animation), AnimationPriority (High))
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction (state, target))
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (..))
import PotatoCactus.Game.Entity.Interaction.Target (InteractionTarget (NpcTarget, ObjectTarget), NpcInteractionType (NpcAttack))
import PotatoCactus.Game.Entity.Npc.Npc (Npc (definitionId))
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import PotatoCactus.Game.Entity.Npc.NpcMovement (doMovement)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject (Added))
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject, facingDirection))
import PotatoCactus.Game.Entity.Object.GameObjectKey (GameObjectKey (GameObjectKey))
import PotatoCactus.Game.Message.RegisterClientPayload (RegisterClientPayload (player))
import PotatoCactus.Game.Movement.PositionXY (fromXY)
import PotatoCactus.Game.Player (Player (serverIndex))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (x, z))
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (DropItemEvent, NpcAttackEvent, NpcCannotReachTargetEvent, NpcDeadEvent, NpcEntityTickEvent, PlayerAttackEvent, PlayerInteractionEvent), ScriptActionResult (AddGameObject, ClearPlayerInteraction, DispatchAttackNpcToPlayer, DispatchAttackPlayerToNpc, InternalRemoveNpcTargetReferences, NpcMoveTowardsTarget, NpcSetAnimation, RemoveItemStack))
import PotatoCactus.Game.Typing (key)
import PotatoCactus.Game.World (World (tick))

dispatchScriptEvent :: World -> GameEvent -> IO [ScriptActionResult]
dispatchScriptEvent world (PlayerInteractionEvent player interaction) =
  trace
    ("Dispatched interaction event " ++ show interaction)
    ( case (target interaction, state interaction) of
        (NpcTarget npcId NpcAttack, InProgress) ->
          return
            [ DispatchAttackPlayerToNpc (serverIndex player) npcId (Hit 0 MeleeAttack),
              ClearPlayerInteraction (serverIndex player)
            ]
        _ -> return []
    )
dispatchScriptEvent world (NpcCannotReachTargetEvent npc target) =
  return [NpcMoveTowardsTarget npc]
dispatchScriptEvent world (PlayerAttackEvent player target) =
  trace
    "dispatched attack event"
    ( case target of
        Combat.NpcTarget npcId ->
          return
            [ DispatchAttackPlayerToNpc (serverIndex player) npcId (Hit 1 MeleeAttack)
            ]
        _ -> return [ClearPlayerInteraction (serverIndex player)]
    )
dispatchScriptEvent world (NpcAttackEvent npc target) =
  trace
    "dispatched NPC attack event"
    ( case target of
        Combat.PlayerTarget targetPlayer ->
          return
            [ DispatchAttackNpcToPlayer (NPC.serverIndex npc) targetPlayer (Hit 0 MeleeAttack),
              NpcSetAnimation (NPC.serverIndex npc) (Animation 309 0 High)
            ]
        _ -> return []
    )
dispatchScriptEvent world (NpcDeadEvent npc) =
  trace
    "dispatched NPC dead event"
    ( return
        [ NpcSetAnimation (NPC.serverIndex npc) (Animation 2607 0 High),
          InternalRemoveNpcTargetReferences (NPC.serverIndex npc)
        ]
    )
dispatchScriptEvent _ _ = return []
