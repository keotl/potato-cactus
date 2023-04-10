module PotatoCactus.Game.Scripting.ProcessTickUpdates (dispatchScriptEvents) where

import PotatoCactus.Game.Scripting.Bridge.Communication (readScriptResult, sendEventsAsync)
import PotatoCactus.Game.Scripting.Events.ApplyScriptActionResult (applyScriptResult)
import PotatoCactus.Game.Scripting.Events.CreateGameEvents (createGameEvents)
import PotatoCactus.Game.Scripting.MockScriptInteractions (dispatchScriptEvent)
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent, ScriptActionResult (InternalProcessingComplete))
import PotatoCactus.Game.World (World)
import PotatoCactus.Interop.ScriptEngineProcess (ScriptEngineHandle, getInstance)

dispatchScriptEvents :: World -> IO World
dispatchScriptEvents world = do
  let events = createGameEvents world

  -- Dispatch all events with the same view of World.
  -- This means that multiple events could result in conflicting action results.
  -- To prevent this, we could either
  --     1. Process scripts sequentially. There probably is a significant performance
  --        penalty in doing this. This would however yield the most accurate result.
  --     2. Add conflict resolution logic in the applyScriptResult function. Whichever
  --        action goes first would then be treated as right.
  -- A possible issue witch conflicting actions might be NPC pathing.
  sendEventsAsync events
  scriptResults <- mapM (dispatchScriptEvent world) events
  bridge <- getInstance

  let withBuiltinScriptResults = foldl applyScriptResult world (concat scriptResults)
  readBridgeEventsUntilDone_ withBuiltinScriptResults bridge

dispatchAndApply_ :: World -> GameEvent -> IO World
dispatchAndApply_ world event = do
  scriptResult <- dispatchScriptEvent world event
  return (foldl applyScriptResult world scriptResult)

readBridgeEventsUntilDone_ :: World -> ScriptEngineHandle -> IO World
readBridgeEventsUntilDone_ world bridge = do
  message <- readScriptResult bridge
  case message of
    InternalProcessingComplete -> return world
    action -> readBridgeEventsUntilDone_ (applyScriptResult world action) bridge
