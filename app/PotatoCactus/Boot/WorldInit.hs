{-# LANGUAGE BangPatterns #-}

module PotatoCactus.Boot.WorldInit (initializeWorld) where

import GHC.IORef (readIORef, writeIORef)
import PotatoCactus.Game.Definitions.StaticGameObjectSet (getStaticObjectSetInstance, objectAt)
import qualified PotatoCactus.Game.Definitions.StaticGameObjectSet as StaticSet
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import qualified PotatoCactus.Game.Entity.Npc.RespawnStrategy as Respawn
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as DynamicObjectCollection
import qualified PotatoCactus.Game.Movement.Pathing.CollisionMapBuilder as CollisionMapBuilder
import qualified PotatoCactus.Game.Movement.Pathing.TileFlagsMap as TileFlagsMap
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Game.World (World (collisionMap, objects, staticObjectLookup_), addNpc, worldInstance)

initializeWorld :: IO ()
initializeWorld = do
  staticObjectSet <- getStaticObjectSetInstance
  defaultWorld <- readIORef worldInstance
  let world =
        defaultWorld
          { objects = DynamicObjectCollection.create (objectAt staticObjectSet),
            collisionMap = CollisionMapBuilder.buildCollisionMap (StaticSet.allEntries staticObjectSet),
            staticObjectLookup_ = StaticSet.findObjectById staticObjectSet
          }

  -- Force strict evaluation of the collision map expression before the game thread starts
  let !_ = TileFlagsMap.getTileFlags (Position 0 0 0) (collisionMap world)

  writeIORef
    worldInstance
    world
