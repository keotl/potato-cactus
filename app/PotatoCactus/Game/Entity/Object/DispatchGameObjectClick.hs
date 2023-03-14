module PotatoCactus.Game.Entity.Object.DispatchGameObjectClick where

import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject (Added), addDynamicObject)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (ObjectClickMessage))
import PotatoCactus.Game.Message.ObjectClickPayload (ObjectClickPayload (ObjectClickPayload, objectId, position))
import PotatoCactus.Game.Movement.PositionXY (fromXY)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (z))
import PotatoCactus.Game.World (World (objects))

dispatchGameObjectClick :: String -> ObjectClickPayload -> World -> World
dispatchGameObjectClick playerName payload world =
  case objectId payload of
    1530 ->
      world
        { objects =
            addDynamicObject
              (Added $ GameObject 1531 (fromXY (position payload) 0) 0) -- TODO - read facing from static set and plane from player  - keotl 2023-03-13
              (objects world)
        }
    1531 ->
      world
        { objects =
            addDynamicObject
              (Added $ GameObject 1530 (fromXY (position payload) 0) 3) -- TODO - read facing from static set and plane from player  - keotl 2023-03-13
              (objects world)
        }
    _ -> world
