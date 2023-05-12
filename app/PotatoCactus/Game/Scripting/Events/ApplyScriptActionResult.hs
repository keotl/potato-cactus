module PotatoCactus.Game.Scripting.Events.ApplyScriptActionResult (applyScriptResult) where

import Debug.Trace (trace)
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity (target), CombatTarget (NpcTarget, PlayerTarget), clearTarget)
import qualified PotatoCactus.Game.Entity.Animation.Animation as Anim
import qualified PotatoCactus.Game.Entity.EntityData as EntityData
import PotatoCactus.Game.Entity.Interaction.Interaction (create)
import PotatoCactus.Game.Entity.Npc.Npc (Npc (respawn))
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import PotatoCactus.Game.Entity.Npc.NpcMovement (immediatelyQueueMovement)
import qualified PotatoCactus.Game.Entity.Npc.NpcMovement as NM
import PotatoCactus.Game.Entity.Npc.RespawnStrategy (RespawnStrategy (Never), respawning)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (addDynamicObject)
import PotatoCactus.Game.Movement.PathPlanner (findPath, findPathNaive)
import PotatoCactus.Game.Player (Player (interaction), clearTargetIfEngagedWithNpc)
import qualified PotatoCactus.Game.Player as P
import qualified PotatoCactus.Game.PlayerUpdate.PlayerAnimationDefinitions as PAnim
import PotatoCactus.Game.Position (GetPosition (getPosition))
import qualified PotatoCactus.Game.Scripting.Actions.SpawnNpcRequest as SpawnReq
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (ScriptInvokedEvent), ScriptActionResult (AddGameObject, ClearPlayerInteraction, CreateInterface, DispatchAttackNpcToPlayer, DispatchAttackPlayerToNpc, InternalNoop, InternalProcessingComplete, InternalRemoveNpcTargetReferences, InvokeScript, NpcMoveTowardsTarget, NpcQueueWalk, NpcSetAnimation, NpcSetForcedChat, SendMessage, ServerPrintMessage, SetPlayerEntityData, SetPlayerPosition, SpawnNpc, UpdateNpc))
import PotatoCactus.Game.World (World (npcs, objects, players))
import qualified PotatoCactus.Game.World as W
import PotatoCactus.Game.World.MobList (findByIndex, remove, updateAll, updateAtIndex)
import PotatoCactus.Game.World.Selectors (isNpcAt)
import PotatoCactus.Utils.Flow ((|>))

applyScriptResult :: World -> ScriptActionResult -> World
applyScriptResult world (AddGameObject obj) =
  world
    { objects = addDynamicObject obj (objects world)
    }
-- applyScriptResult world (UpdatePlayer playerId p) =
--   world
--     { players = updateAtIndex (players world) playerId (const p)
--     }
applyScriptResult world (UpdateNpc npcId npc) =
  world
    { npcs = updateAtIndex (npcs world) npcId (const npc)
    }
applyScriptResult world (ClearPlayerInteraction playerId) =
  world
    { players = updateAtIndex (players world) playerId (\p -> p {interaction = create, P.combat = clearTarget . P.combat $ p})
    }
applyScriptResult world (DispatchAttackPlayerToNpc srcPlayer targetNpc hit) =
  world
    { npcs = updateAtIndex (npcs world) targetNpc (NPC.applyHit (PlayerTarget srcPlayer) hit),
      players =
        updateAtIndex
          (players world)
          srcPlayer
          ( \p ->
              p
                |> P.setAttackCooldown
                |> P.setAttackTarget (NpcTarget targetNpc)
                |> P.setAnimation (PAnim.attackAnimation p)
          )
    }
applyScriptResult world (DispatchAttackNpcToPlayer srcNpc targetPlayer hit) =
  world
    { players =
        updateAtIndex
          (players world)
          targetPlayer
          ( \p ->
              p
                |> P.applyHit (NpcTarget srcNpc) hit
                |> P.setAnimation (PAnim.defenceAnimation p)
          ),
      npcs =
        updateAtIndex
          (npcs world)
          srcNpc
          ( \n ->
              NPC.setAttackTarget (NPC.setAttackCooldown n) (PlayerTarget targetPlayer)
          )
    }
applyScriptResult world (NpcSetAnimation npcIndex anim) =
  world
    { npcs = updateAtIndex (npcs world) npcIndex (`NPC.setAnimation` anim)
    }
applyScriptResult world (NpcMoveTowardsTarget npc) =
  case target . NPC.combat $ npc of
    PlayerTarget playerId ->
      case findByIndex (W.players world) playerId of
        Just p ->
          case findPathNaive 666 (getPosition npc) (getPosition p) of
            [] -> world
            (desiredMove : _) ->
              if isNpcAt world desiredMove
                then world
                else
                  world
                    { npcs =
                        updateAtIndex
                          (W.npcs world)
                          (NPC.serverIndex npc)
                          ( \npc ->
                              npc
                                { NPC.movement =
                                    immediatelyQueueMovement
                                      (NPC.movement npc)
                                      [desiredMove]
                                }
                          )
                    }
        Nothing -> world
    _ -> world
applyScriptResult world (InternalRemoveNpcTargetReferences npcId) =
  world
    { players = updateAll (W.players world) (clearTargetIfEngagedWithNpc npcId)
    }
applyScriptResult world (SpawnNpc options) =
  let respawn = case SpawnReq.respawnDelay options of
        -1 -> Never
        delay -> respawning (SpawnReq.position options) delay
   in W.addNpc world (NPC.create (SpawnReq.npcId options) (SpawnReq.position options) respawn)
applyScriptResult world (NpcQueueWalk npcIndex pos) =
  world
    { npcs =
        updateAtIndex
          (W.npcs world)
          npcIndex
          ( \npc ->
              npc
                { NPC.movement =
                    NM.immediatelyQueueMovement
                      (NPC.movement npc)
                      (findPath 666 (getPosition npc) pos)
                }
          )
    }
applyScriptResult world (NpcSetForcedChat npcIndex msg) =
  world
    { npcs =
        updateAtIndex
          (W.npcs world)
          npcIndex
          (`NPC.setForcedChat` msg)
    }
applyScriptResult world InternalNoop = world
applyScriptResult world InternalProcessingComplete = world
applyScriptResult world (ServerPrintMessage message) = trace message world
applyScriptResult world (SendMessage playerIndex message) =
  world
    { players = updateAtIndex (players world) playerIndex (`P.sendChatboxMessage` message)
    }
applyScriptResult world (SetPlayerPosition playerIndex pos) =
  world
    { players = updateAtIndex (players world) playerIndex (`P.setPosition` pos)
    }
applyScriptResult world (InvokeScript invocation) =
  W.queueEvent world (ScriptInvokedEvent invocation)
applyScriptResult world (CreateInterface playerIndex request) =
  world
    { players = updateAtIndex (players world) playerIndex (`P.createInterface` request)
    }
applyScriptResult world (SetPlayerEntityData playerIndex key val) =
  world
    { players =
        updateAtIndex
          (players world)
          playerIndex
          (`P.updateEntityData` (\d -> EntityData.setValue d key val))
    }
