module PotatoCactus.Game.World where

import Control.Concurrent (Chan)
import Data.IORef (newIORef)
import Data.List (find)
import GHC.IO (unsafePerformIO)
import PotatoCactus.Config.Constants (maxNpcs, maxPlayers)
import PotatoCactus.Game.Entity.Npc.AdvanceNpc (advanceNpc)
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObjectCollection (DynamicObjectCollection), create)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject)
import PotatoCactus.Game.Message.ObjectClickPayload (ObjectClickPayload)
import PotatoCactus.Game.Player (PlayerIndex)
import qualified PotatoCactus.Game.Player as P (Player (serverIndex), create, username)
import PotatoCactus.Game.PlayerUpdate.AdvancePlayer (advancePlayer)
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Game.Typing (Advance (advance))
import PotatoCactus.Game.World.EntityPositionFinder (combatTargetPosOrDefault)
import PotatoCactus.Game.World.MobList (MobList, add, create, findByIndex, findByPredicate, remove, updateAll, updateAtIndex, updateByPredicate)
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
    npcs :: MobList NPC.Npc,
    clients :: [ClientHandle],
    objects :: DynamicObjectCollection
  }
  deriving (Show)

instance Advance World where
  advance w =
    let newNpcs =
          updateAll
            (npcs w)
            ( advanceNpc (combatTargetPosOrDefault (players w) (npcs w)) 666
            )
     in w
          { tick = tick w + 1,
            players = updateAll (players w) (advancePlayer (findByIndex newNpcs)),
            npcs = newNpcs
          }

defaultWorldValue :: World
defaultWorldValue =
  World
    { tick = 0,
      players = PotatoCactus.Game.World.MobList.create maxPlayers,
      npcs = PotatoCactus.Game.World.MobList.create maxNpcs,
      clients = [],
      objects = PotatoCactus.Game.Entity.Object.DynamicObjectCollection.create
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

updatePlayerByIndex :: World -> PlayerIndex -> (P.Player -> P.Player) -> World
updatePlayerByIndex world playerId update =
  world
    { players =
        updateAtIndex
          (players world)
          playerId
          update
    }

addPlayer :: World -> P.Player -> World
addPlayer world player =
  case add (players world) player of
    Left (newPlayers, index) ->
      world {players = updateAtIndex newPlayers index (\p -> p {P.serverIndex = index})}
    Right _ -> world

removePlayerByUsername :: World -> String -> World
removePlayerByUsername world username =
  case findByPredicate (players world) (\p -> P.username p == username) of
    Just p -> world {players = remove (players world) (P.serverIndex p)}
    Nothing -> world

addNpc :: World -> NPC.Npc -> World
addNpc world npc =
  case add (npcs world) npc of
    Left (newNpcs, index) ->
      world {npcs = updateAtIndex newNpcs index (\n -> n {NPC.serverIndex = index})}
    Right _ -> world
