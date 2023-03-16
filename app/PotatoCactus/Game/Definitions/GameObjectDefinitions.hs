module PotatoCactus.Game.Definitions.GameObjectDefinitions where

import Data.IORef (IORef, readIORef, writeIORef)
import Data.IntMap (IntMap, empty, insert, (!?))
import GHC.IO (unsafePerformIO)
import GHC.IORef (newIORef)

type GameObjectId = Int

-- type GameObjectType = Int

data GameObjectDefinition = GameObjectDefinition
  { id :: GameObjectId
  -- objectType :: GameObjectType
  }
  deriving (Show)

objectDb :: IORef (IntMap GameObjectDefinition)
{-# NOINLINE objectDb #-}
objectDb = unsafePerformIO $ newIORef empty

objectDefinition :: GameObjectId -> Maybe GameObjectDefinition
objectDefinition objectId =
  unsafePerformIO
    ( do
        db <- readIORef objectDb
        return (db !? objectId)
    )

initializeObjectDb :: IO Int
initializeObjectDb = do
  db <- readIORef objectDb
  let updated =
        foldl
          (\a e -> e a)
          db
          [ addMockObject_ 5553,
            addMockObject_ 1530,
            addMockObject_ 1531
          ]
  writeIORef objectDb updated
  return $ length updated

addMockObject_ :: GameObjectId -> (IntMap GameObjectDefinition -> IntMap GameObjectDefinition)
addMockObject_ objectId =
  insert objectId (GameObjectDefinition objectId)
