{-# LANGUAGE BangPatterns #-}

module PotatoCactus.Boot.WorldInit (initializeWorld) where

import GHC.IORef (readIORef, writeIORef)
import PotatoCactus.Game.Definitions.StaticGameObjectSet (getStaticObjectSetInstance, objectAt)
import qualified PotatoCactus.Game.Definitions.StaticGameObjectSet as StaticSet
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as DynamicObjectCollection
import qualified PotatoCactus.Game.Movement.Pathing.CollisionMap as CollisionMap
import qualified PotatoCactus.Game.Movement.Pathing.CollisionMapBuilder as CollisionMapBuilder
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Game.World (World (collisionMap, objects, staticObjectSet), addNpc, worldInstance)

initializeWorld :: IO ()
initializeWorld = do
  staticObjectSet <- getStaticObjectSetInstance
  defaultWorld <- readIORef worldInstance
  let world =
        defaultWorld
          { objects =
              DynamicObjectCollection.create
                (objectAt staticObjectSet)
                (StaticSet.objectsInRegion staticObjectSet),
            collisionMap =
              CollisionMapBuilder.buildCollisionMap
                (StaticSet.allEntries staticObjectSet),
            staticObjectSet = staticObjectSet
          }

  -- Force strict evaluation of the collision map expression before the game thread starts
  let !_ = CollisionMap.allowsMovementBetween (Position 0 0 0) (Position 1 0 0) (collisionMap world)

  writeIORef
    worldInstance
    world
