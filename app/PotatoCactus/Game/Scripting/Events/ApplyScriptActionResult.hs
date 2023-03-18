module PotatoCactus.Game.Scripting.Events.ApplyScriptActionResult (applyScriptResult) where

import PotatoCactus.Game.Entity.Interaction.Interaction (create)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (addDynamicObject)
import PotatoCactus.Game.Player (Player (interaction))
import PotatoCactus.Game.Scripting.ScriptUpdates (ScriptActionResult (AddGameObject, ClearPlayerInteraction, UpdateNpc, UpdatePlayer))
import PotatoCactus.Game.World (World (npcs, objects, players))
import PotatoCactus.Game.World.MobList (updateAtIndex)

applyScriptResult :: World -> ScriptActionResult -> World
applyScriptResult world (AddGameObject obj) =
  world
    { objects = addDynamicObject obj (objects world)
    }
applyScriptResult world (UpdatePlayer playerId p) =
  world
    { players = updateAtIndex (players world) playerId (const p)
    }
applyScriptResult world (UpdateNpc npcId npc) =
  world
    { npcs = updateAtIndex (npcs world) npcId (const npc)
    }
applyScriptResult world (ClearPlayerInteraction playerId) =
  world
    { players = updateAtIndex (players world) playerId (\p -> p {interaction = create})
    }
