module PotatoCactus.Game.Scripting.MockScriptInteractions where

import Debug.Trace (trace)
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction (state, target))
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (..))
import PotatoCactus.Game.Entity.Interaction.Target (InteractionTarget (ObjectTarget))
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject (Added))
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Entity.Object.GameObjectKey (GameObjectKey (GameObjectKey))
import PotatoCactus.Game.Message.RegisterClientPayload (RegisterClientPayload (player))
import PotatoCactus.Game.Movement.PositionXY (fromXY)
import PotatoCactus.Game.Player (Player (serverIndex))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (z))
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (PlayerInteraction), ScriptActionResult (AddGameObject, ClearPlayerInteraction))
import PotatoCactus.Game.World (World)

dispatchScriptEvent :: World -> GameEvent -> IO [ScriptActionResult]
dispatchScriptEvent world (PlayerInteraction player interaction) =
  trace
    ("Dispatched interaction event " ++ (show interaction))
    ( case (target interaction, state interaction) of
        (ObjectTarget (GameObjectKey 1530 pos) 1, InProgress) ->
          return
            [ ClearPlayerInteraction (serverIndex player),
              AddGameObject (Added $ GameObject 1531 pos 0) -- TODO - read facing from static set  - keotl 2023-03-14
            ]
        (ObjectTarget (GameObjectKey 1531 pos) 1, InProgress) ->
          return
            [ ClearPlayerInteraction (serverIndex player),
              AddGameObject (Added $ GameObject 1530 pos 3) -- TODO - read facing from static set  - keotl 2023-03-14
            ]
        _ -> return [ClearPlayerInteraction (serverIndex player)]
    )

-- dispatchTickUpdate _ _ = return []
