module PotatoCactus.Boot.GameThreadMain where

import Control.Concurrent (Chan, forkFinally, readChan, threadDelay, writeChan)
import Data.IORef
import Data.Typeable (typeOf)
import GHC.Clock (getMonotonicTimeNSec)
import PotatoCactus.Boot.GameChannel (GameChannelMessage (UpdateWorldMessage), gameChannel)
import PotatoCactus.Config.Constants (tickInterval)
import PotatoCactus.Game.Reducer (reduceWorld)
import PotatoCactus.Game.World (ClientHandle (controlChannel, username), ClientHandleMessage (WorldUpdatedMessage), World (clients), defaultWorldValue, worldInstance)

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
  -- print "going to next tick!"

  -- TODO - remove this dummy print  - keotl 2022-11-30
  putStr "connected clients:"
  let x = clients newWorld
  case x of
    [] -> print "[]"
    c -> print (username $ head c)

  writeIORef worldInstance newWorld
  notifyClients_ $ clients newWorld
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
      UpdateWorldMessage -> return world
      x -> reduceUntilNextTick_ (reduceWorld world x) gameChannel
    )

notifyClients_ :: [ClientHandle] -> IO ()
notifyClients_ [] = do
  return ()
notifyClients_ clients = do
  let c = head clients
  writeChan (controlChannel c) WorldUpdatedMessage
  notifyClients_ $ tail clients
