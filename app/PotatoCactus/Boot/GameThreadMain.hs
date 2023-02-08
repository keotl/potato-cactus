module PotatoCactus.Boot.GameThreadMain where

import Control.Concurrent (Chan, forkFinally, readChan, threadDelay, writeChan)
import Data.IORef
import Data.Typeable (typeOf)
import GHC.Clock (getMonotonicTimeNSec)
import PotatoCactus.Boot.GameChannel (GameChannelMessage (UpdateWorldMessage), gameChannel)
import PotatoCactus.Config.Constants (tickInterval)
import PotatoCactus.Game.Reducer (reduceWorld)
import PotatoCactus.Game.World (ClientHandle (controlChannel, username), ClientHandleMessage (CloseClientConnectionMessage, WorldUpdatedMessage), World (clients), defaultWorldValue, worldInstance)
import qualified PotatoCactus.Game.World as W

gameThreadMain :: IO ()
gameThreadMain = do
  putStrLn "Started game thread."

  worldTickThreadId <- forkFinally (worldTickThread_ tickInterval gameChannel) (\x -> print "worldTick thread exited")
  mainLoop

  return ()

mainLoop :: IO ()
mainLoop = do
  startTime <- getMonotonicTimeNSec
  world <- readIORef worldInstance
  newWorld <- reduceUntilNextTick_ world gameChannel

  print newWorld

  writeIORef worldInstance newWorld
  notifyClients_ WorldUpdatedMessage (clients newWorld)
  mainLoop

worldTickThread_ :: Int -> Chan GameChannelMessage -> IO ()
worldTickThread_ tickInterval gameChannel = do
  threadDelay tickInterval
  writeChan gameChannel UpdateWorldMessage
  worldTickThread_ tickInterval gameChannel

reduceUntilNextTick_ :: World -> Chan GameChannelMessage -> IO World
reduceUntilNextTick_ world gameChannel = do
  message <- readChan gameChannel

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
