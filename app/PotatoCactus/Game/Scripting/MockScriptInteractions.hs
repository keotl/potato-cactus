module PotatoCactus.Game.Scripting.MockScriptInteractions where

import Debug.Trace (trace)
import PotatoCactus.Game.Definitions.StaticGameObjectSet (staticObjectAt)
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction (state, target))
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (..))
import PotatoCactus.Game.Entity.Interaction.Target (InteractionTarget (ObjectTarget))
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import PotatoCactus.Game.Entity.Npc.NpcMovement (doMovement)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject (Added))
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject, facingDirection))
import PotatoCactus.Game.Entity.Object.GameObjectKey (GameObjectKey (GameObjectKey))
import PotatoCactus.Game.Message.RegisterClientPayload (RegisterClientPayload (player))
import PotatoCactus.Game.Movement.PositionXY (fromXY)
import PotatoCactus.Game.Player (Player (serverIndex))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (x, z))
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (NpcEntityTick, PlayerInteraction), ScriptActionResult (AddGameObject, ClearPlayerInteraction, UpdateNpc))
import PotatoCactus.Game.Typing (key)
import PotatoCactus.Game.World (World (tick))

dispatchScriptEvent :: World -> GameEvent -> IO [ScriptActionResult]
dispatchScriptEvent world (PlayerInteraction player interaction) =
  trace
    ("Dispatched interaction event " ++ show interaction)
    ( case (target interaction, state interaction) of
        (ObjectTarget (GameObjectKey 1530 pos) 1, InProgress) ->
          return (ClearPlayerInteraction (serverIndex player) : openDoor_ pos)
        (ObjectTarget (GameObjectKey 1531 pos) 1, InProgress) ->
          return (ClearPlayerInteraction (serverIndex player) : closeDoor_ pos)
        _ -> return [ClearPlayerInteraction (serverIndex player)]
    )
dispatchScriptEvent world (NpcEntityTick npc) =
  return
    ( case key npc of
        "hans" ->
          [ UpdateNpc
              (NPC.serverIndex npc)
              ( npc
                  { NPC.movement =
                      doMovement
                        (NPC.movement npc)
                        ((getPosition npc) {x = x (getPosition npc) + 1})
                  }
              )
            | tick world `mod` 8 == 0
          ]
        _ -> []
    )

-- dispatchTickUpdate _ _ = return []

objDirection_ :: Position -> Int -> Int
objDirection_ pos objType =
  case staticObjectAt pos objType of
    Just obj -> trace ("got static object:" ++ show obj) facingDirection obj
    Nothing -> 0

openDoor_ :: Position -> [ScriptActionResult]
openDoor_ pos =
  case staticObjectAt pos 0 of
    Just normalDoor ->
      [AddGameObject (Added $ GameObject 1531 pos 0 ((objDirection_ pos 0 + 1) `mod` 4))]
    Nothing ->
      ( case staticObjectAt pos 9 of
          Just diagonalDoor ->
            [ AddGameObject (Added $ GameObject 1531 pos 9 ((objDirection_ pos 0 + 1) `mod` 4))
            ]
          _ -> []
      )

closeDoor_ :: Position -> [ScriptActionResult]
closeDoor_ pos =
  case staticObjectAt pos 0 of
    Just normalDoor ->
      [AddGameObject (Added $ GameObject 1530 pos 0 (objDirection_ pos 0))]
    Nothing ->
      ( case staticObjectAt pos 9 of
          Just diagonalDoor ->
            -- TODO - We might need to group together multiple object types. e.g. interactible object group  - keotl 2023-03-16
            [ AddGameObject (Added $ GameObject 1530 pos 9 (objDirection_ pos 0))
            ]
          _ -> []
      )
