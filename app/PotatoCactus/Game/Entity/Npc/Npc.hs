module PotatoCactus.Game.Entity.Npc.Npc where

type NpcIndex = Int

data Npc = Npc
  { serverIndex :: NpcIndex
  }
  deriving (Show)
