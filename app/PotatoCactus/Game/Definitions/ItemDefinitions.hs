module PotatoCactus.Game.Definitions.ItemDefinitions (ItemDefinition(..), ItemId, itemDefinition, initializeDb) where

import Data.IntMap (IntMap, empty, insert, size, (!?))
import GHC.IO (unsafePerformIO)
import GHC.IORef (IORef, newIORef, readIORef, writeIORef)
import Prelude hiding (id)

type ItemId = Int

data ItemDefinition = ItemDefinition
  { id :: ItemId,
    name :: String,
    stackable :: Bool
    -- weight :: Int
    -- equipSlot :: Int, -- -1 if not equipable
    -- equippedShouldHideModel_ :: Bool
  }

itemDb :: IORef (IntMap ItemDefinition)
{-# NOINLINE itemDb #-}
itemDb = unsafePerformIO $ newIORef empty

itemDefinition :: ItemId -> Maybe ItemDefinition
itemDefinition itemId =
  unsafePerformIO
    ( do
        db <- readIORef itemDb
        return (db !? itemId)
    )

initializeDb :: IO Int
initializeDb = do
  db <- readIORef itemDb
  -- TODO - read from file  - keotl 2023-03-01
  let updated =
        foldl
          (\a e -> e a)
          db
          [ addMockItem_ 1115 "Iron platebody" False,
            addMockItem_ 1067 "Iron platelegs" False,
            addMockItem_ 1137 "Iron med helm" False,
            addMockItem_ 1155 "Bronze full helm" False,
            addMockItem_ 1947 "Grain" False,
            addMockItem_ 617 "Coins" True
          ]
  writeIORef itemDb updated

  return (size updated)

addMockItem_ :: Int -> String -> Bool -> (IntMap ItemDefinition -> IntMap ItemDefinition)
addMockItem_ itemId name stackable =
  insert itemId (ItemDefinition itemId name stackable)
