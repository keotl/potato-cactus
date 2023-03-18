module PotatoCactus.Game.Scripting.Events.NpcEvents where

import PotatoCactus.Game.Entity.Npc.Npc (Npc)
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (NpcEntityTick))

createNpcEvents :: Npc -> [GameEvent]
createNpcEvents npc =
  [NpcEntityTick npc]
