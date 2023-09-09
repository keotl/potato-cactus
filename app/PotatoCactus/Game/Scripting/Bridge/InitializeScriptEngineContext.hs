module PotatoCactus.Game.Scripting.Bridge.InitializeScriptEngineContext (initializeScriptEngineContext) where

import Data.IORef (readIORef, writeIORef)
import PotatoCactus.Game.Scripting.Bridge.ControlMessages (doneSendingEventsMessage, updateWorldContextMessage)
import PotatoCactus.Game.Scripting.Bridge.Serialization.GameEventMapper (mapEvent)
import PotatoCactus.Game.Scripting.ProcessTickUpdates (readBridgeEventsUntilDone)
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (ServerInitEvent))
import PotatoCactus.Game.World (World, worldInstance)
import PotatoCactus.Interop.ScriptEngineProcess (ScriptEngineHandle, getInstance, send)

initializeScriptEngineContext :: IO ()
initializeScriptEngineContext = do
  handle <- getInstance
  world <- readIORef worldInstance

  send handle $ Just (updateWorldContextMessage world)
  send handle (mapEvent ServerInitEvent)
  send handle $ Just doneSendingEventsMessage

  newWorld <- readBridgeEventsUntilDone world handle
  writeIORef worldInstance newWorld
  return ()
