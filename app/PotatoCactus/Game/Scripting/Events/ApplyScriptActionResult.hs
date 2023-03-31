module PotatoCactus.Game.Scripting.Events.ApplyScriptActionResult (applyScriptResult) where

import PotatoCactus.Game.Combat.CombatEntity (CombatEntity (target), CombatTarget (NpcTarget, PlayerTarget))
import qualified PotatoCactus.Game.Entity.Animation.Animation as Anim
import PotatoCactus.Game.Entity.Interaction.Interaction (create)
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import PotatoCactus.Game.Entity.Npc.NpcMovement (immediatelyQueueMovement)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (addDynamicObject)
import PotatoCactus.Game.Movement.PathPlanner (findPathNaive)
import PotatoCactus.Game.Player (Player (interaction))
import qualified PotatoCactus.Game.Player as P
import qualified PotatoCactus.Game.PlayerUpdate.PlayerAnimationDefinitions as PAnim
import PotatoCactus.Game.Position (GetPosition (getPosition))
import PotatoCactus.Game.Scripting.ScriptUpdates (ScriptActionResult (AddGameObject, ClearPlayerInteraction, DispatchAttackNpcToPlayer, DispatchAttackPlayerToNpc, NpcMoveTowardsTarget, NpcSetAnimation, UpdateNpc))
import PotatoCactus.Game.World (World (npcs, objects, players))
import qualified PotatoCactus.Game.World as W
import PotatoCactus.Game.World.MobList (findByIndex, updateAtIndex)
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
    { players = updateAtIndex (players world) playerId (\p -> p {interaction = create})
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
