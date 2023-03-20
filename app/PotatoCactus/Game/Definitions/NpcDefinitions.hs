module PotatoCactus.Game.Definitions.NpcDefinitions where

import Data.IORef (IORef, readIORef, writeIORef)
import Data.IntMap (IntMap, empty, insert, (!?))
import GHC.IO (unsafePerformIO)
import GHC.IORef (newIORef)

type NpcDefinitionId = Int

data NpcDefinition = NpcDefinition
  { id :: NpcDefinitionId,
    hitpoints :: Int
  }
  deriving (Show)

npcDb :: IORef (IntMap NpcDefinition)
{-# NOINLINE npcDb #-}
npcDb = unsafePerformIO $ newIORef empty

npcDefinition :: NpcDefinitionId -> Maybe NpcDefinition
npcDefinition objectId =
  unsafePerformIO
    ( do
        db <- readIORef npcDb
        return (db !? objectId)
    )

initializeNpcDb :: IO Int
initializeNpcDb = do
  db <- readIORef npcDb
  let updated =
        foldl
          (\a e -> e a)
          db
          [ addMockNpc_ 100 5
          ]
  writeIORef npcDb updated
  return $ length updated

addMockNpc_ :: NpcDefinitionId -> Int -> (IntMap NpcDefinition -> IntMap NpcDefinition)
addMockNpc_ npcId hitpoints =
  insert npcId (NpcDefinition npcId hitpoints)
