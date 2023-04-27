module PotatoCactus.Boot.WorldInit (initializeWorld) where

import GHC.IORef (readIORef, writeIORef)
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import qualified PotatoCactus.Game.Entity.Npc.RespawnStrategy as Respawn
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Game.World (World, addNpc, worldInstance)

initializeWorld :: IO ()
initializeWorld = do
  defaultWorld <- readIORef worldInstance
  let world = initialize_ defaultWorld
  writeIORef worldInstance world

initialize_ :: World -> World
initialize_ w =
  foldl
    addNpc
    w
    [
      -- NPC.create 0 (Position 3093 3240 0) Respawn.Never
      -- NPC.create 100 (Position 3093 3250 0) (Respawn.respawning (Position 3093 3250 0) 10)
    ]
