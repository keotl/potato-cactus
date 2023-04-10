module PotatoCactus.Game.Scripting.Bridge.Communication (sendEventsAsync, readScriptResult) where

import Control.Concurrent (forkFinally)
import PotatoCactus.Game.Scripting.Bridge.MapEvents (mapEvent)
import PotatoCactus.Game.Scripting.Bridge.MapResults (mapResult)
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent, ScriptActionResult)
import PotatoCactus.Interop.ScriptEngineProcess (ScriptEngineHandle, getInstance, recv, send)
import PotatoCactus.Utils.Logging (LogLevel (Debug, Fatal, Info), logger)

sendEvents :: ScriptEngineHandle -> [GameEvent] -> IO ()
sendEvents _ [] = return ()
sendEvents handle (x : xs) = do
  send handle (mapEvent x)
  sendEvents handle xs

sendEventsAsync :: [GameEvent] -> IO ()
sendEventsAsync evts = do
  threadId <-
    forkFinally
      (sendEventsThreadMain_ evts)
      ( \x ->
          case x of
            Left exception -> logger_ Fatal $ "Sending events via engine bridge failed with '" ++ show exception ++ "'."
            _ -> return ()
      )

  return ()

sendEventsThreadMain_ :: [GameEvent] -> IO ()
sendEventsThreadMain_ evts = do
  handle <- getInstance
  sendEvents handle evts
  logger_ Debug $ "Sent " ++ show (length evts) ++ " game events."
  send handle "DONE"
  return ()

logger_ = logger "ScriptingBridge"

readScriptResult :: ScriptEngineHandle -> IO ScriptActionResult
readScriptResult handle = do
  res <- recv handle
  return (mapResult res)
