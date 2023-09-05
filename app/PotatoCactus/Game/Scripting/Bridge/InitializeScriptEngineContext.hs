module PotatoCactus.Game.Scripting.Bridge.InitializeScriptEngineContext (initializeScriptEngineContext) where

import Data.IORef (readIORef, writeIORef)
import PotatoCactus.Game.Definitions.StaticGameObjectSet (getStaticObjectSetInstance)
import PotatoCactus.Game.Scripting.Bridge.ControlMessages (doneSendingEventsMessage, setStaticObjectSetMessage, updateWorldContextMessage)
import PotatoCactus.Game.Scripting.Bridge.Serialization.GameEventMapper (mapEvent)
import PotatoCactus.Game.Scripting.ProcessTickUpdates (readBridgeEventsUntilDone)
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (ServerInitEvent))
import PotatoCactus.Game.World (World, worldInstance)
import PotatoCactus.Interop.ScriptEngineProcess (ScriptEngineHandle, getInstance, send)

initializeScriptEngineContext :: IO ()
initializeScriptEngineContext = do
  handle <- getInstance
  world <- readIORef worldInstance

  -- TODO - Send static definitions  - keotl 2023-04-27
  -- staticObjects <- getStaticObjectSetInstance
  -- send handle (setStaticObjectSetMessage staticObjects)

  send handle (updateWorldContextMessage world)
  send handle (mapEvent ServerInitEvent)
  send handle doneSendingEventsMessage

  newWorld <- readBridgeEventsUntilDone world handle
  writeIORef worldInstance newWorld
  return ()
