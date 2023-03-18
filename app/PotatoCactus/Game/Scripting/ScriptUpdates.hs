module PotatoCactus.Game.Scripting.ScriptUpdates where

import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction)
import PotatoCactus.Game.Entity.Npc.Npc (Npc, NpcIndex)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject)
import PotatoCactus.Game.Player (Player, PlayerIndex)

data GameEvent
  = PlayerInteraction Player Interaction
  | NpcEntityTick Npc

data ScriptActionResult
  = UpdatePlayer PlayerIndex Player
  | AddGameObject DynamicObject
  | ClearPlayerInteraction PlayerIndex
  | UpdateNpc NpcIndex Npc
