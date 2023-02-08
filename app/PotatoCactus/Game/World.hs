module PotatoCactus.Game.World where

import Control.Concurrent (Chan)
import Data.IORef (newIORef)
import GHC.IO (unsafePerformIO)
import PotatoCactus.Game.Player (Player)

data ClientHandleMessage = WorldUpdatedMessage | CloseClientConnectionMessage

data ClientHandle = ClientHandle
  { username :: String,
    controlChannel :: Chan ClientHandleMessage
  }

instance Show ClientHandle where
  show x = username x

data World = World
  { tick :: Int,
    players :: [Player],
    clients :: [ClientHandle]
  }
  deriving (Show)

defaultWorldValue = World {tick = 0, players = [], clients = []}

worldInstance = unsafePerformIO $ newIORef defaultWorldValue
{-# NOINLINE worldInstance #-}
