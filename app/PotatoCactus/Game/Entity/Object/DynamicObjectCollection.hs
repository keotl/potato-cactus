module PotatoCactus.Game.Entity.Object.DynamicObjectCollection where

import Data.IntMap (IntMap, alter, delete, empty, findWithDefault, toList)
import Data.IntMap.Lazy (insert)
import Data.Maybe (fromMaybe)
import PotatoCactus.Config.Constants (chunkBoundExponentXY_)
import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectId)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (id, objectType, position), GameObjectType, gameObjectHash)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position, x, y, z), chunkX, chunkY)
import qualified PotatoCactus.Game.Position as Pos
import Prelude hiding (id)

data DynamicObject = Added GameObject | Removed GameObject deriving (Eq, Show)

instance GetPosition DynamicObject where
  getPosition (Added object) = getPosition object
  getPosition (Removed object) = getPosition object

data DynamicObjectCollection = DynamicObjectCollection
  { elements_ :: IntMap (IntMap [DynamicObject])
  }
  deriving (Show)

create :: DynamicObjectCollection
create = DynamicObjectCollection empty

addDynamicObject :: GameObject -> DynamicObjectCollection -> DynamicObjectCollection
addDynamicObject object collection =
  let updatedElements =
        alter
          (insertInChunkMap_ object)
          (objChunkKey_ object)
          (elements_ collection)
   in collection {elements_ = updatedElements}

insertInChunkMap_ :: DynamicObject -> Maybe (IntMap [DynamicObject]) -> Maybe (IntMap [DynamicObject])
insertInChunkMap_ obj chunkMap =
  Just $
    alter
      (alterChunkMapInsert_ obj)
      (key_ obj)
      (fromMaybe empty chunkMap)

alterChunkMapInsert_ :: DynamicObject -> Maybe [DynamicObject] -> Maybe [DynamicObject]
alterChunkMapInsert_ newObj Nothing = Just [newObj]
alterChunkMapInsert_ newObj (Just l) = Just (newObj : l)

removeDynamicObject :: GameObjectId -> (Position, GameObjectType) -> DynamicObjectCollection -> DynamicObjectCollection
removeDynamicObject objectId (pos, objType) collection =
  let updated =
        alter
          (removeFromChunkMap_ objectId (pos, objType))
          (posChunkKey_ pos)
          (elements_ collection)
   in collection {elements_ = updated}

removeFromChunkMap_ :: GameObjectId -> (Position, GameObjectType) -> Maybe (IntMap [DynamicObject]) -> Maybe (IntMap [DynamicObject])
removeFromChunkMap_ _ _ Nothing = Nothing
removeFromChunkMap_ objectId keylike (Just chunkMap) =
  Just $
    alter
      (alterChunkMapRemove_ objectId keylike)
      (gameObjectHash keylike)
      chunkMap

alterChunkMapRemove_ :: GameObjectId -> (Position, GameObjectType) -> Maybe [DynamicObject] -> Maybe [DynamicObject]
alterChunkMapRemove_ _ (pos, objType) Nothing = Nothing
alterChunkMapRemove_ removedId (pos, objType) (Just l) =
  Just $
    filter
      (not . matchesForRemoval_ removedId)
      l

matchesForRemoval_ :: GameObjectId -> DynamicObject -> Bool
matchesForRemoval_ objectId (Added obj) = id obj == objectId
matchesForRemoval_ objectId (Removed obj) = id obj == objectId

key_ :: DynamicObject -> Int
key_ (Added object) = gameObjectHash (position object, objectType object)
key_ (Removed object) = gameObjectHash (position object, objectType object)

chunkKey_ :: Int -> Int -> Int -> Int
chunkKey_ cx cy z =
  cx
    + cy * 10 ^ chunkBoundExponentXY_
    + z * 10 ^ (2 * chunkBoundExponentXY_)

posChunkKey_ :: Position -> Int
posChunkKey_ pos =
  let cx = chunkX pos in let cy = chunkY pos in chunkKey_ cx cy (z pos)

objChunkKey_ :: DynamicObject -> Int
objChunkKey_ obj =
  posChunkKey_ (getPosition obj)

findByChunkXY :: Int -> Int -> Int -> DynamicObjectCollection -> [DynamicObject]
findByChunkXY cx cy z collection =
  let chunkMap =
        findWithDefault
          empty
          (chunkKey_ cx cy z)
          (elements_ collection)
   in concatMap snd (toList chunkMap)

-- For serialization
iter :: DynamicObjectCollection -> [DynamicObject]
iter collection =
  let chunkMaps = map snd $ toList (elements_ collection)
   in concat $
        concatMap
          (map snd . toList)
          chunkMaps
