module PotatoCactus.Game.World where

import Data.IORef (newIORef)
import GHC.IO (unsafePerformIO)
import PotatoCactus.Game.Player (Player)
import Control.Concurrent (Chan)

data ClientHandleMessage = WorldUpdatedMessage

data ClientHandle = ClientHandle
  { username :: String,
    controlChannel :: Chan ClientHandleMessage
  }

data World = World
  { tick :: Int,
    players :: [Player],
    clients :: [ClientHandle]
  }

defaultWorldValue = World {tick = 0, players = [], clients = []}

worldInstance = unsafePerformIO $ newIORef defaultWorldValue
{-# NOINLINE worldInstance #-}
