module PotatoCactus.Game.Scripting.Events.ApplyScriptActionResult (applyScriptResult) where

import Data.Maybe (isJust)
import Debug.Trace (trace)
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity (target), CombatTarget (NpcTarget, PlayerTarget), clearTarget)
import qualified PotatoCactus.Game.Entity.Animation.Animation as Anim
import qualified PotatoCactus.Game.Entity.EntityData as EntityData
import qualified PotatoCactus.Game.Entity.GroundItem.GroundItemCollection as GroundItemCollection
import PotatoCactus.Game.Entity.Interaction.Interaction (create)
import qualified PotatoCactus.Game.Entity.Interaction.Interaction as Interaction
import qualified PotatoCactus.Game.Entity.Interaction.State as InteractionState
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import PotatoCactus.Game.Entity.Npc.NpcMovement (immediatelyQueueMovement)
import qualified PotatoCactus.Game.Entity.Npc.NpcMovement as NM
import PotatoCactus.Game.Entity.Npc.RespawnStrategy (RespawnStrategy (Never), respawning)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (addDynamicObject, removeDynamicObject)
import qualified PotatoCactus.Game.ItemContainer as ItemContainer
import qualified PotatoCactus.Game.Movement.MovementEntity as PM
import PotatoCactus.Game.Movement.PathPlanner (findPath, findPathNaive)
import PotatoCactus.Game.Player (Player (interaction))
import qualified PotatoCactus.Game.Player as P
import qualified PotatoCactus.Game.PlayerUpdate.PlayerAnimationDefinitions as PAnim
import PotatoCactus.Game.PlayerUpdate.VarpSet (Varp (varpId))
import PotatoCactus.Game.Position (GetPosition (getPosition))
import qualified PotatoCactus.Game.Scripting.Actions.SpawnNpcRequest as SpawnReq
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (ScriptInvokedEvent), ScriptActionResult (..))
import PotatoCactus.Game.World (World (npcs, objects, players), groundItems)
import qualified PotatoCactus.Game.World as W
import PotatoCactus.Game.World.MobList (findByIndex, remove, updateAll, updateAtIndex)
import PotatoCactus.Game.World.Selectors (isNpcAt)
import PotatoCactus.Utils.Flow ((|>))

applyScriptResult :: World -> ScriptActionResult -> World
applyScriptResult world (SpawnGameObject obj) =
  world
    { objects = addDynamicObject obj (objects world)
    }
applyScriptResult world (RemoveGameObject obj) =
  world
    { objects = removeDynamicObject obj (objects world)
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
applyScriptResult world (InvokeScript invocation delay) =
  W.scheduleCallback world invocation (W.tick world + max delay 1)
applyScriptResult world (CreateInterface playerIndex request) =
  world
    { players = updateAtIndex (players world) playerIndex (`P.createInterface` request)
    }
applyScriptResult world (ClearStandardInterface playerIndex) =
  world
    { players = updateAtIndex (players world) playerIndex P.clearStandardInterface
    }
applyScriptResult world (SetPlayerEntityData playerIndex key val) =
  world
    { players =
        updateAtIndex
          (players world)
          playerIndex
          (`P.updateEntityData` (\d -> EntityData.setValue d key val))
    }
applyScriptResult world (SetPlayerAnimation playerIndex anim) =
  world
    { players = updateAtIndex (players world) playerIndex (P.setAnimation anim)
    }
applyScriptResult world (GiveItem playerIndex itemId quantity) =
  world
    { players = updateAtIndex (players world) playerIndex (`P.giveItem` ItemContainer.ItemStack itemId quantity)
    }
applyScriptResult world (SubtractItem playerIndex itemId quantity) =
  world
    { players = updateAtIndex (players world) playerIndex (`P.subtractItem` (itemId, quantity))
    }
applyScriptResult world (RemoveItemStack playerIndex itemId index) =
  world
    { players = updateAtIndex (players world) playerIndex (`P.removeItemStack` (itemId, index))
    }
applyScriptResult world (SpawnGroundItem item) =
  world
    { groundItems = GroundItemCollection.insert (groundItems world) item
    }
applyScriptResult world (RemoveGroundItem itemId quantity position removedByPlayer) =
  let playerKey = case removedByPlayer of
        Nothing -> Nothing
        Just playerId -> fmap P.username (findByIndex (players world) playerId)
   in let (removed, updated) =
            GroundItemCollection.remove
              (groundItems world)
              (itemId, quantity, position, playerKey)
       in case (removedByPlayer, removed) of
            (_, ItemContainer.Empty) -> world
            (Nothing, _) ->
              world
                { groundItems = updated
                }
            (Just playerIndex, item) ->
              case findByIndex (players world) playerIndex of
                Nothing -> world
                Just player ->
                  if ItemContainer.canAddItem (P.inventory player) item
                    then
                      world
                        { groundItems = updated,
                          players =
                            updateAtIndex
                              (players world)
                              playerIndex
                              (`P.giveItem` item)
                        }
                    else
                      world
                        { players =
                            updateAtIndex
                              (players world)
                              playerIndex
                              ( `P.sendChatboxMessage`
                                  "You need more inventory space to carry that item."
                              )
                        }
applyScriptResult world (SetPlayerVarp playerIndex operation) =
  world
    { players =
        updateAtIndex
          (players world)
          playerIndex
          (`P.setVarp` operation)
    }
applyScriptResult world (SetPlayerVarbit playerIndex operation) =
  world
    { players =
        updateAtIndex
          (players world)
          playerIndex
          (`P.setVarbit` operation)
    }
applyScriptResult world (PlayerQueueWalk playerIndex targetPos) =
  world
    { players =
        updateAtIndex
          (W.players world)
          playerIndex
          ( \player ->
              player
                { P.movement =
                    PM.immediatelyQueueMovement
                      (P.movement player)
                      (findPath 666 (getPosition player) targetPos)
                }
          )
    }
applyScriptResult world (InternalSetPlayerInteractionPending playerIndex) =
  world
    { players =
        updateAtIndex
          (W.players world)
          playerIndex
          ( \player ->
              player
                { P.interaction =
                    (P.interaction player) {Interaction.state = InteractionState.Pending}
                }
          )
    }
