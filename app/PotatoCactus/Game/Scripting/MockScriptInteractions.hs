module PotatoCactus.Game.Scripting.MockScriptInteractions where

import Debug.Trace (trace)
import PotatoCactus.Game.Definitions.StaticGameObjectSet (staticObjectAt)
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction (state, target))
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (..))
import PotatoCactus.Game.Entity.Interaction.Target (InteractionTarget (ObjectTarget))
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject (Added))
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject, facingDirection))
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
              AddGameObject (Added $ GameObject 1531 pos (((objDirection_ pos 0) +1) `mod` 4))
            ]
        (ObjectTarget (GameObjectKey 1531 pos) 1, InProgress) ->
          return
            [ ClearPlayerInteraction (serverIndex player),
              AddGameObject (Added $ GameObject 1530 pos (((objDirection_ pos 0))))
            ]
        _ -> return [ClearPlayerInteraction (serverIndex player)]
    )

-- dispatchTickUpdate _ _ = return []

objDirection_ :: Position -> Int -> Int
objDirection_ pos objType =
  case staticObjectAt pos objType of
    Just obj -> trace ("got static object:" ++ (show obj) ) facingDirection obj
    Nothing -> 0
