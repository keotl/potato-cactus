module PotatoCactus.Boot.GameThreadMain where

import Control.Concurrent (Chan, forkFinally, readChan, threadDelay, writeChan)
import Data.IORef (readIORef, writeIORef)
import Data.Typeable (typeOf)
import GHC.Clock (getMonotonicTimeNSec)
import PotatoCactus.Boot.GameChannel (gameChannel)
import PotatoCactus.Config.Constants (tickInterval)
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (ObjectClickMessage, UpdateWorldMessage))
import PotatoCactus.Game.Reducer (reduceWorld)
import PotatoCactus.Game.Scripting.ProcessTickUpdates (dispatchScriptEvents)
import PotatoCactus.Game.World (ClientHandle (controlChannel, username), ClientHandleMessage (CloseClientConnectionMessage, WorldUpdatedMessage), World (clients, tick), defaultWorldValue, worldInstance)
import qualified PotatoCactus.Game.World as W
import PotatoCactus.Utils.Logging (LogLevel (Debug, Fatal, Info, Warning), logger)

gameThreadMain :: IO ()
gameThreadMain = do
  logger_ Info "Started game thread."

  worldTickThreadId <-
    forkFinally
      (worldTickThread_ tickInterval gameChannel)
      (\x -> logger_ Fatal $ "WorldTick thread exited with '" ++ show x ++ "'.")

  mainLoop
  return ()

mainLoop :: IO ()
mainLoop = do
  startTime <- getMonotonicTimeNSec
  world <- readIORef worldInstance
  newWorld <- reduceUntilNextTick_ world gameChannel
  newWorld2 <- dispatchScriptEvents newWorld

  -- logger_ Info $ (show newWorld2)

  writeIORef worldInstance newWorld2
  -- TODO - Investigate blocking IO for freeze on player disconnect bug  - keotl 2023-03-27
  -- appears to be fixed with -threaded flag
  notifyClients_ WorldUpdatedMessage (clients newWorld2)
  mainLoop

worldTickThread_ :: Int -> Chan GameChannelMessage -> IO ()
worldTickThread_ tickInterval gameChannel = do
  threadDelay tickInterval
  writeChan gameChannel UpdateWorldMessage
  worldTickThread_ tickInterval gameChannel

reduceUntilNextTick_ :: World -> Chan GameChannelMessage -> IO World
reduceUntilNextTick_ world gameChannel = do
  message <- readChan gameChannel

  -- TESTING PRINT
  -- case message of
  --   ObjectClickMessage username payload -> do
  --     print username
  --     print payload
  --   _ -> putStr ""
  -- END TESTING PRINT

  ( case message of
      UpdateWorldMessage -> return (reduceWorld world message)
      x -> reduceUntilNextTick_ (reduceWorld world x) gameChannel
    )

notifyClients_ :: ClientHandleMessage -> [ClientHandle] -> IO ()
notifyClients_ _ [] = do
  return ()
notifyClients_ message clients = do
  let c = head clients
  writeChan (controlChannel c) message
  notifyClients_ message (tail clients)

logger_ = logger "GameThread"
