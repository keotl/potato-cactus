module PotatoCactus.Boot.GameThreadMain where

import Control.Concurrent (readChan)
import Data.IORef
import Data.Typeable (typeOf)
import PotatoCactus.Boot.GameChannel (gameChannel)
import PotatoCactus.Game.World (World, defaultWorldValue)

gameThreadMain :: IO ()
gameThreadMain = do
  putStrLn "Started game thread."

  world <- newIORef defaultWorldValue

  mainLoop

  return ()

mainLoop :: IO ()
mainLoop = do
  -- channel <- readIORef gameChannelRef
  message <- readChan gameChannel

  print $ typeOf message
  mainLoop
