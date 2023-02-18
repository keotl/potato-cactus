module PotatoCactus.Game.World where

import Control.Concurrent (Chan)
import Data.IORef (newIORef)
import Data.List (find)
import GHC.IO (unsafePerformIO)
import PotatoCactus.Config.Constants (maxPlayers)
import qualified PotatoCactus.Game.Player as P (Player, create, username)
import PotatoCactus.Game.PlayerUpdate.AdvancePlayer (advancePlayer)
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Game.Typing (Advance (advance))
import PotatoCactus.Game.World.MobList (MobList, add, create, updateAll, updateByPredicate)
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
    players :: MobList P.Player,
    clients :: [ClientHandle]
  }
  deriving (Show)

instance Advance World where
  advance w =
    w
      { tick = tick w + 1,
        players = updateAll (players w) advancePlayer
      }

defaultWorldValue =
  World
    { tick = 0,
      players =
        case add (create maxPlayers) mockPlayer_ of -- TODO - replace with simply (create maxPlayers)  - keotl 2023-02-18
          Left (a, _) -> a
          Right _ -> create maxPlayers,
      clients = []
    }

worldInstance = unsafePerformIO $ newIORef defaultWorldValue
{-# NOINLINE worldInstance #-}

updatePlayer :: World -> String -> (P.Player -> P.Player) -> World
updatePlayer world playerName update =
  world
    { players =
        updateByPredicate
          (players world)
          (\x -> P.username x == playerName)
          update
    }

mockPlayer_ :: P.Player
mockPlayer_ = P.create "the doctor" (Position 3093 3254 0)
