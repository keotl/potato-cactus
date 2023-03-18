module PotatoCactus.Boot.WorldInit (initializeWorld) where

import GHC.IORef (readIORef, writeIORef)
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Game.World (World, addNpc, worldInstance)

initializeWorld :: IO ()
initializeWorld = do
  defaultWorld <- readIORef worldInstance
  let world = initialize_ defaultWorld
  writeIORef worldInstance world

initialize_ :: World -> World
initialize_ w =
  addNpc w (NPC.create 0 "hans" (Position 3093 3240 0))
