module PotatoCactus.Game.Scripting.Events.CreateGameEvents (createGameEvents) where

import PotatoCactus.Game.Scripting.Events.NpcEvents (createNpcEvents)
import PotatoCactus.Game.Scripting.Events.PlayerEvents (createPlayerEvents)
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent)
import PotatoCactus.Game.World (World (npcs, players, triggeredEvents))
import PotatoCactus.Game.World.MobList (iter)

createGameEvents :: World -> [GameEvent]
createGameEvents world =
  concat
    [ concatMap createPlayerEvents (iter . players $ world),
      concatMap createNpcEvents (iter . npcs $ world),
      triggeredEvents world
    ]
