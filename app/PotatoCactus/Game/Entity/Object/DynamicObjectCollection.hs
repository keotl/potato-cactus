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
          (addObjectInChunkMap_ object)
          (objChunkKey_ object)
          (elements_ collection)
   in collection {elements_ = updatedElements}

addObjectInChunkMap_ :: GameObject -> Maybe (IntMap [DynamicObject]) -> Maybe (IntMap [DynamicObject])
addObjectInChunkMap_ obj chunkMap =
  Just $
    alter
      (addObjectToTile_ obj)
      (key_ obj)
      (fromMaybe empty chunkMap)

addObjectToTile_ :: GameObject -> Maybe [DynamicObject] -> Maybe [DynamicObject]
addObjectToTile_ newObject Nothing = Just [Added newObject]
addObjectToTile_ newObject (Just tileObjects) =
  case matchingEntry_ newObject tileObjects of
    Nothing -> Just $ Added newObject : tileObjects
    Just existing -> case existing of
      Added _ -> Just tileObjects
      Removed _ -> Just $ filter ((/=) newObject . unwrap_) tileObjects

matchingEntry_ :: GameObject -> [DynamicObject] -> Maybe DynamicObject
matchingEntry_ _ [] = Nothing
matchingEntry_ predicateObject (x : xs) =
  let object = unwrap_ x
   in if predicateObject == object
        then Just x
        else matchingEntry_ predicateObject xs

removeDynamicObject :: GameObject -> DynamicObjectCollection -> DynamicObjectCollection
removeDynamicObject object collection =
  let updated =
        alter
          (removeObjectFromChunkMap_ object)
          (objChunkKey_ object)
          (elements_ collection)
   in collection {elements_ = updated}

removeObjectFromChunkMap_ :: GameObject -> Maybe (IntMap [DynamicObject]) -> Maybe (IntMap [DynamicObject])
removeObjectFromChunkMap_ object Nothing =
  Just $
    insert (key_ object) [Removed object] empty
removeObjectFromChunkMap_ object (Just chunkMap) =
  Just $
    alter
      (removeFromTileObjects_ object)
      (key_ object)
      chunkMap

removeFromTileObjects_ :: GameObject -> Maybe [DynamicObject] -> Maybe [DynamicObject]
removeFromTileObjects_ removed Nothing = Just [Removed removed]
removeFromTileObjects_ removed (Just tileObjects) =
  case matchingEntry_ removed tileObjects of
    Nothing -> Just $ Removed removed : tileObjects
    Just existing -> case existing of
      Added _ -> Just $ filter ((/=) removed . unwrap_) tileObjects
      Removed _ -> Just tileObjects

key_ :: GameObject -> Int
key_ object = gameObjectHash (position object, objectType object)

chunkKey_ :: Int -> Int -> Int -> Int
chunkKey_ cx cy z =
  cx
    + cy * 10 ^ chunkBoundExponentXY_
    + z * 10 ^ (2 * chunkBoundExponentXY_)

posChunkKey_ :: Position -> Int
posChunkKey_ pos =
  let cx = chunkX pos in let cy = chunkY pos in chunkKey_ cx cy (z pos)

objChunkKey_ :: GetPosition a => a -> Int
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

unwrap_ :: DynamicObject -> GameObject
unwrap_ (Added obj) = obj
unwrap_ (Removed obj) = obj

-- For serialization
iter :: DynamicObjectCollection -> [DynamicObject]
iter collection =
  let chunkMaps = map snd $ toList (elements_ collection)
   in concat $
        concatMap
          (map snd . toList)
          chunkMaps
