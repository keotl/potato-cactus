module PotatoCactus.Game.Definitions.ItemDefinitions (itemDefinition, initializeDb) where

import Data.IntMap (IntMap, empty, insert, size, (!?))
import GHC.IO (unsafePerformIO)
import GHC.IORef (IORef, newIORef, readIORef, writeIORef)
import PotatoCactus.Game.Definitions.Parser.ItemDefinitionParser (parseItemDefinitionFile)
import PotatoCactus.Game.Definitions.Types.ItemDefinition (ItemDefinition (id), ItemId)
import Prelude hiding (id)

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

initializeDb :: String -> IO Int
initializeDb definitionsFile = do
  db <- readIORef itemDb
  extracted <- parseItemDefinitionFile definitionsFile
  let updated =
        foldl
          (\a e -> e a)
          db
          (map addItem_ extracted)
  writeIORef itemDb updated

  return (size updated)

addItem_ :: ItemDefinition -> (IntMap ItemDefinition -> IntMap ItemDefinition)
addItem_ item =
  insert (id item) item
