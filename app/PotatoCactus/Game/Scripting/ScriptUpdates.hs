module PotatoCactus.Game.Scripting.ScriptUpdates where

import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject)
import PotatoCactus.Game.Player (Player, PlayerIndex)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject)

data GameEvent
  = PlayerInteraction Player Interaction

data ScriptActionResult
  = UpdatePlayer PlayerIndex Player
  | AddGameObject DynamicObject
  | ClearPlayerInteraction PlayerIndex
