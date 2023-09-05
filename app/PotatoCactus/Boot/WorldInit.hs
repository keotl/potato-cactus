module PotatoCactus.Boot.WorldInit (initializeWorld) where

import GHC.IORef (readIORef, writeIORef)
import PotatoCactus.Game.Definitions.StaticGameObjectSet (getStaticObjectSetInstance, objectAt)
import qualified PotatoCactus.Game.Definitions.StaticGameObjectSet as StaticSet
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import qualified PotatoCactus.Game.Entity.Npc.RespawnStrategy as Respawn
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Game.World (World (objects, staticObjectLookup_), addNpc, worldInstance)

initializeWorld :: IO ()
initializeWorld = do
  staticObjectSet <- getStaticObjectSetInstance
  defaultWorld <- readIORef worldInstance
  let world = defaultWorld
  writeIORef
    worldInstance
    ( world
        { objects =
            PotatoCactus.Game.Entity.Object.DynamicObjectCollection.create
              (objectAt staticObjectSet),
          staticObjectLookup_ = StaticSet.findObjectById staticObjectSet
        }
    )
