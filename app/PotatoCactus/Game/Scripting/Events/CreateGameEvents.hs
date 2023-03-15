module PotatoCactus.Game.Scripting.Events.CreateGameEvents (createGameEvents) where

import PotatoCactus.Game.Scripting.Events.PlayerEvents (createPlayerEvents)
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent)
import PotatoCactus.Game.World (World (players))
import PotatoCactus.Game.World.MobList (iter)

createGameEvents :: World -> [GameEvent]
createGameEvents world =
  concat
    [ concatMap createPlayerEvents (iter . players $ world)
    ]
