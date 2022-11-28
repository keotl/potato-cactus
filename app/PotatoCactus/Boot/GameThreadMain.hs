module PotatoCactus.Boot.GameThreadMain where

import Control.Concurrent (Chan, forkFinally, readChan, threadDelay, writeChan)
import Data.IORef
import Data.Typeable (typeOf)
import GHC.Clock (getMonotonicTimeNSec)
import PotatoCactus.Boot.GameChannel (GameChannelMessage (UpdateWorldMessage), gameChannel)
import PotatoCactus.Config.Constants (tickInterval)
import PotatoCactus.Game.Reducer (reduceWorld)
import PotatoCactus.Game.World (World, defaultWorldValue, worldInstance)

gameThreadMain :: IO ()
gameThreadMain = do
  putStrLn "Started game thread."

  worldTickThreadId <- forkFinally (worldTickThread_ tickInterval gameChannel) (\x -> print "worldTick thread exited")
  mainLoop

  return ()

mainLoop :: IO ()
mainLoop = do
  -- TODO read non-blocking until it is time to go to next tick
  -- https://hackage.haskell.org/package/synchronous-channels-0.2/docs/Control-Concurrent-Chan-Synchronous.html#v:writeList2Chan
  -- tryReadChan
  startTime <- getMonotonicTimeNSec
  world <- readIORef worldInstance
  newWorld <- reduceUntilNextTick_ world gameChannel
  print "going to next tick!"
  -- message <- readChan gameChannel

  -- print $ typeOf message
  -- TODO increment tick
  -- TODO - forEach connected client, send "worldUpdated notification"
  -- asyncWriteChan
  mainLoop

worldTickThread_ :: Int -> Chan GameChannelMessage -> IO ()
worldTickThread_ tickInterval gameChannel = do
  threadDelay tickInterval
  writeChan gameChannel UpdateWorldMessage
  worldTickThread_ tickInterval gameChannel

reduceUntilNextTick_ :: World -> Chan GameChannelMessage -> IO World
reduceUntilNextTick_ world gameChannel = do
  message <- readChan gameChannel
  print $ typeOf message
  let next = reduceWorld world message

  ( case message of
      UpdateWorldMessage -> return world
      x -> reduceUntilNextTick_ next gameChannel
    )
