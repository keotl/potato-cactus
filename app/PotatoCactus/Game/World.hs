module PotatoCactus.Game.World where

import Control.Concurrent (Chan)
import Data.IORef (IORef, newIORef, writeIORef)
import Data.List (find)
import GHC.IO (unsafePerformIO)
import PotatoCactus.Config.Constants (maxNpcs, maxPlayers)
import PotatoCactus.Game.Definitions.StaticGameObjectSet (staticObjectAt)
import PotatoCactus.Game.Entity.GroundItem.GroundItemCollection (GroundItemCollection)
import qualified PotatoCactus.Game.Entity.GroundItem.GroundItemCollection as GroundItemCollection
import PotatoCactus.Game.Entity.Npc.AdvanceNpc (advanceNpc)
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObjectCollection, create)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject)
import PotatoCactus.Game.Message.ObjectClickPayload (ObjectClickPayload)
import PotatoCactus.Game.Player (PlayerIndex)
import qualified PotatoCactus.Game.Player as P (Player (serverIndex), create, username)
import PotatoCactus.Game.PlayerUpdate.AdvancePlayer (advancePlayer)
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Game.Scripting.Actions.ScriptInvocation (ScriptInvocation)
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (ScriptInvokedEvent))
import PotatoCactus.Game.Typing (Advance (advance), ShouldDiscard (shouldDiscard))
import PotatoCactus.Game.World.CallbackScheduler (CallbackScheduler)
import qualified PotatoCactus.Game.World.CallbackScheduler as Scheduler
import PotatoCactus.Game.World.EntityPositionFinder (combatTargetPosOrDefault)
import PotatoCactus.Game.World.MobList (MobList, add, create, findByIndex, findByPredicate, remove, removeByPredicate, updateAll, updateAtIndex, updateByPredicate)
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
    objects :: DynamicObjectCollection,
    groundItems :: GroundItemCollection,
    triggeredEvents :: [GameEvent], -- Additional events to dispatch on this tick. For events not tied to a specific entity.
    pendingEvents_ :: [GameEvent], -- Additional events to dispatch on the next tick.
    scheduler :: CallbackScheduler
  }
  deriving (Show)

instance Advance World where
  advance w =
    let newNpcs =
          updateAll
            (npcs w)
            ( advanceNpc (combatTargetPosOrDefault (players w) (npcs w))
            )
     in w
          { tick = tick w + 1,
            players = updateAll (players w) (advancePlayer (findByIndex newNpcs)),
            npcs = removeByPredicate newNpcs shouldDiscard,
            groundItems = GroundItemCollection.advanceTime (groundItems w) (tick w + 1),
            triggeredEvents = pendingEvents_ w ++ invokedScripts_ w,
            pendingEvents_ = [],
            scheduler = Scheduler.clearCallbacksForTick (scheduler w) (tick w + 1)
          }

invokedScripts_ :: World -> [GameEvent]
invokedScripts_ w =
  map ScriptInvokedEvent $ Scheduler.callbacksForTick (scheduler w) (tick w + 1)

defaultWorldValue :: World
defaultWorldValue =
  World
    { tick = 0,
      players = PotatoCactus.Game.World.MobList.create maxPlayers,
      npcs = PotatoCactus.Game.World.MobList.create maxNpcs,
      clients = [],
      objects = PotatoCactus.Game.Entity.Object.DynamicObjectCollection.create staticObjectAt,
      groundItems = GroundItemCollection.create,
      triggeredEvents = [],
      pendingEvents_ = [],
      scheduler = Scheduler.create
    }

worldInstance :: IORef World
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

queueEvent :: World -> GameEvent -> World
queueEvent w e =
  w {pendingEvents_ = e : pendingEvents_ w}

scheduleCallback :: World -> ScriptInvocation -> Int -> World
scheduleCallback w script tick =
  w
    { scheduler = Scheduler.queueCallback (scheduler w) script tick
    }
