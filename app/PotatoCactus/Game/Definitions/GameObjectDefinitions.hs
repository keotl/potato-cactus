module PotatoCactus.Game.Definitions.GameObjectDefinitions where

import Data.IORef (IORef, readIORef)
import Data.IntMap (IntMap, empty, insert, (!?))
import GHC.IO (unsafePerformIO)
import GHC.IORef (newIORef)

type GameObjectId = Int

data GameObjectDefinition = GameObjectDefinition
  { id :: GameObjectId,
    objectType :: Int
  }

objectDb :: IORef (IntMap GameObjectDefinition)
{-# NOINLINE objectDb #-}
objectDb = unsafePerformIO $ newIORef empty

itemDefinition :: GameObjectId -> Maybe GameObjectDefinition
itemDefinition objectId =
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
          [ addMockObject_ 5553 10,
            addMockObject_ 1530 0,
            addMockObject_ 1531 0
          ]
  return $ length db

addMockObject_ :: GameObjectId -> Int -> (IntMap GameObjectDefinition -> IntMap GameObjectDefinition)
addMockObject_ objectId objectType =
  insert objectId (GameObjectDefinition objectId objectType)
