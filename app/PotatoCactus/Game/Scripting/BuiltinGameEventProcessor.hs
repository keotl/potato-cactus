module PotatoCactus.Game.Scripting.BuiltinGameEventProcessor where

import Data.Maybe (isJust, fromJust)
import Debug.Trace (trace)
import qualified PotatoCactus.Game.Combat.CombatEntity as Combat
import PotatoCactus.Game.Combat.Hit (DamageType (MeleeAttack), Hit (Hit))
import PotatoCactus.Game.Definitions.StaticGameObjectSet (getStaticObjectSetInstance, objectAt)
import PotatoCactus.Game.Entity.Animation.Animation (Animation (Animation), AnimationPriority (High))
import PotatoCactus.Game.Entity.Interaction.AdvanceInteractionDeps (findClosestInteractableTile)
import PotatoCactus.Game.Entity.Interaction.ClosestInteractableTileCalc (selectClosestInteractableTile)
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction (state, target))
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (..))
import PotatoCactus.Game.Entity.Interaction.Target (InteractionTarget (NpcTarget, ObjectTarget))
import PotatoCactus.Game.Entity.Npc.Npc (Npc (definitionId))
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject, facingDirection))
import PotatoCactus.Game.Message.RegisterClientPayload (RegisterClientPayload (player))
import PotatoCactus.Game.Movement.Pathing.PathPlanner (findPathNaive)
import PotatoCactus.Game.Movement.PositionXY (fromXY)
import PotatoCactus.Game.Player (Player (serverIndex))
import qualified PotatoCactus.Game.Player as P
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position, x, z))
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (DropItemEvent, InternalNpcCannotReachCombatTargetEvent, InternalPlayerCannotReachCombatTargetEvent, InternalPlayerInteractionPendingPathingEvent, NpcAttackEvent, NpcDeadEvent, NpcEntityTickEvent, PlayerAttackEvent, PlayerCommandEvent, PlayerInteractionEvent), ScriptActionResult (..))
import PotatoCactus.Game.Typing (key)
import PotatoCactus.Game.World (World (tick))
import qualified PotatoCactus.Game.World as W
import PotatoCactus.Game.World.MobList (findByIndex)

dispatchScriptEvent :: World -> GameEvent -> IO [ScriptActionResult]
dispatchScriptEvent world (InternalPlayerInteractionPendingPathingEvent player target) = do
  let deps = W.createAdvanceInteractionDeps_ world
  return $
    case findClosestInteractableTile deps (getPosition player) target of
      Nothing ->
        [ ClearPlayerInteraction (serverIndex player),
          SendMessage (serverIndex player) "I can't reach that."
        ]
      Just newTargetPos ->
        [ InternalSetPlayerInteractionPending (serverIndex player),
          PlayerQueueWalk (serverIndex player) newTargetPos
        ]
dispatchScriptEvent world (InternalNpcCannotReachCombatTargetEvent npc destination) =
  case findPathNaive 666 (getPosition npc) destination of
    [] -> return []
    firstPathStep : _ -> return [NpcQueueWalk (NPC.serverIndex npc) firstPathStep]
dispatchScriptEvent world (InternalPlayerCannotReachCombatTargetEvent player destination) =
  case findPathNaive 666 (getPosition player) destination of
    [] ->
      return
        [ ClearPlayerInteraction (serverIndex player),
          SendMessage (serverIndex player) "I can't reach that."
        ]
    path -> return [InternalPlayerQueueWalkPath (P.serverIndex player) path]
dispatchScriptEvent world (PlayerAttackEvent player target) =
  trace
    "dispatched attack event"
    ( case target of
        Combat.NpcTarget npcId ->
          return
            [DispatchAttackPlayerToNpc (serverIndex player) npcId (Hit 1 MeleeAttack)]
        _ -> return []
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
        [NpcSetAnimation (NPC.serverIndex npc) (Animation 2607 0 High)]
    )
dispatchScriptEvent world (PlayerCommandEvent playerIndex "listobj" args) = do
  staticSet <- getStaticObjectSetInstance
  let player = findByIndex (W.players world) playerIndex
  let pos = maybe (Position 0 0 0) getPosition player
  let objects = map (\i -> (i, objectAt staticSet pos i)) [0 .. 20]
  let filteredObjs = filter (\(t, obj) -> isJust obj) objects
  if not (null filteredObjs)
    then return [SendMessage playerIndex (show obj) | obj <- filteredObjs]
    else return [SendMessage playerIndex "No static object on current tile."]
dispatchScriptEvent _ _ = return []
