module PotatoCactus.Game.Scripting.Bridge.Communication (sendEventsAsync, readScriptResult) where

import Control.Concurrent (forkFinally)
import PotatoCactus.Game.Scripting.Bridge.ControlMessages (doneSendingEventsMessage, updateWorldContextMessage)
import PotatoCactus.Game.Scripting.Bridge.Serialization.ActionResultMapper (mapResult)
import PotatoCactus.Game.Scripting.Bridge.Serialization.GameEventMapper (mapEvent)
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent, ScriptActionResult (InternalNoop))
import PotatoCactus.Game.World (World)
import PotatoCactus.Interop.ScriptEngineProcess (ScriptEngineHandle, getInstance, recv, send)
import PotatoCactus.Utils.Logging (LogLevel (Debug, Fatal, Info), logger)

sendEvents :: ScriptEngineHandle -> [GameEvent] -> IO ()
sendEvents _ [] = return ()
sendEvents handle (x : xs) = do
  send handle (mapEvent x)
  sendEvents handle xs

sendEventsAsync :: World -> [GameEvent] -> IO ()
sendEventsAsync world evts = do
  threadId <-
    forkFinally
      (sendEventsThreadMain_ world evts)
      ( \x ->
          case x of
            Left exception -> logger_ Fatal $ "Sending events via engine bridge failed with '" ++ show exception ++ "'."
            _ -> return ()
      )

  return ()

sendEventsThreadMain_ :: World -> [GameEvent] -> IO ()
sendEventsThreadMain_ world evts = do
  handle <- getInstance
  send handle (Just $ updateWorldContextMessage world)
  sendEvents handle evts
  logger_ Debug $ "Sent " ++ show (length evts) ++ " game events."
  send handle $ Just doneSendingEventsMessage
  return ()

logger_ = logger "ScriptingBridge"

readScriptResult :: ScriptEngineHandle -> IO ScriptActionResult
readScriptResult handle = do
  res <- recv handle
  return (mapResult res)
