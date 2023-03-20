module PotatoCactus.Game.Scripting.Events.ApplyScriptActionResult (applyScriptResult) where

import PotatoCactus.Game.Combat.CombatEntity (CombatTarget (NpcTarget, PlayerTarget))
import PotatoCactus.Game.Entity.Interaction.Interaction (create)
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (addDynamicObject)
import PotatoCactus.Game.Player (Player (interaction))
import qualified PotatoCactus.Game.Player as P
import PotatoCactus.Game.Scripting.ScriptUpdates (ScriptActionResult (AddGameObject, ClearPlayerInteraction, DispatchAttackNpcToPlayer, DispatchAttackPlayerToNpc, UpdateNpc))
import PotatoCactus.Game.World (World (npcs, objects, players))
import PotatoCactus.Game.World.MobList (updateAtIndex)

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
          (\p -> P.setAttackTarget (P.setAttackCooldown p) (NpcTarget targetNpc))
    }
applyScriptResult world (DispatchAttackNpcToPlayer srcNpc targetPlayer hit) =
  world
    { players = updateAtIndex (players world) targetPlayer (P.applyHit (NpcTarget srcNpc) hit)
    }
