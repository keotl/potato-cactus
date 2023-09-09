module PotatoCactus.Game.Definitions.GameObjectDefinitions where

import Data.IORef (IORef, readIORef, writeIORef)
import Data.IntMap (IntMap, empty, insert, (!?))
import GHC.IO (unsafePerformIO)
import GHC.IORef (newIORef)
import PotatoCactus.Game.Definitions.Parser.ObjectDefinitionParser (parseObjectDefinitionFile)
import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectDefinition (id), GameObjectId)
import Prelude hiding (id)

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

initializeObjectDb :: String -> IO Int
initializeObjectDb definitionsFile = do
  db <- readIORef objectDb
  extracted <- parseObjectDefinitionFile definitionsFile
  let updated =
        foldl
          (\a e -> e a)
          db
          (map addObject_ extracted)
  writeIORef objectDb updated
  return $ length updated

addObject_ :: GameObjectDefinition -> (IntMap GameObjectDefinition -> IntMap GameObjectDefinition)
addObject_ object =
  insert (id object) object
