module PotatoCactus.Game.World where

import Control.Concurrent (Chan)
import Data.IORef (newIORef)
import Data.List (find)
import GHC.IO (unsafePerformIO)
import qualified PotatoCactus.Game.Player as P (Player, username)
import PotatoCactus.Game.PlayerUpdate.AdvancePlayer (advancePlayer)
import PotatoCactus.Game.Typing (Advance (advance))
import PotatoCactus.Utils.Iterable (replace)

data ClientHandleMessage = WorldUpdatedMessage | CloseClientConnectionMessage

data ClientHandle = ClientHandle
  { username :: String,
    controlChannel :: Chan ClientHandleMessage
  }

instance Show ClientHandle where
  show x = username x

data World = World
  { tick :: Int,
    players :: [P.Player],
    clients :: [ClientHandle]
  }
  deriving (Show)

instance Advance World where
  advance w = World (tick w + 1) (map advancePlayer (players w)) (clients w)

defaultWorldValue = World {tick = 0, players = [], clients = []}

worldInstance = unsafePerformIO $ newIORef defaultWorldValue
{-# NOINLINE worldInstance #-}

updatePlayer :: World -> String -> (P.Player -> P.Player) -> World
updatePlayer world playerName update =
  case find (\x -> P.username x == playerName) (players world) of
    Just player ->
      world
        { players =
            replace
              (\x -> P.username x == playerName)
              (update player)
              (players world)
        }
    Nothing -> world
