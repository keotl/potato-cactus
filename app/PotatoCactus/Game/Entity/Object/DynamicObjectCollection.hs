module PotatoCactus.Game.Entity.Object.DynamicObjectCollection where

import Data.IntMap (IntMap, alter, delete, empty, findWithDefault, toList)
import Data.IntMap.Lazy (insert)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import PotatoCactus.Config.Constants (chunkBoundExponentXY_)
import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectId)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (id, objectType, position), GameObjectType, gameObjectHash)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position, x, y, z), chunkX, chunkY)
import qualified PotatoCactus.Game.Position as Pos
import PotatoCactus.Utils.Flow ((|>))
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

addDynamicObject :: (Position -> GameObjectType -> [GameObject]) -> GameObject -> DynamicObjectCollection -> DynamicObjectCollection
addDynamicObject staticObjectAt object collection =
  let updatedElements =
        alter
          (addObjectInChunkMap_ object (staticObjectAt (getPosition object) (objectType object)))
          (objChunkKey_ object)
          (elements_ collection)
   in collection {elements_ = updatedElements}

addObjectInChunkMap_ :: GameObject -> [GameObject] -> Maybe (IntMap [DynamicObject]) -> Maybe (IntMap [DynamicObject])
addObjectInChunkMap_ obj matchingStaticObjects chunkMap =
  Just $
    alter
      (addObjectToTile_ obj matchingStaticObjects)
      (key_ obj)
      (fromMaybe empty chunkMap)

addObjectToTile_ :: GameObject -> [GameObject] -> Maybe [DynamicObject] -> Maybe [DynamicObject]
addObjectToTile_ newObject matchingStaticObjects Nothing = Just $ Added newObject : map Removed matchingStaticObjects
addObjectToTile_ newObject matchingStaticObjects (Just tileObjects) =
  case matchingEntry_ newObject tileObjects of
    Nothing -> Just $ Added newObject : tileObjects ++ map Removed matchingStaticObjects
    (Just existing) -> case existing of
      Added replaced -> Just $ Added newObject : filter ((/=) replaced . unwrap_) tileObjects
      Removed _ -> Just $ filter ((/=) newObject . unwrap_) tileObjects

matchingEntry_ :: GameObject -> [DynamicObject] -> Maybe DynamicObject
matchingEntry_ _ [] = Nothing
matchingEntry_ predicateObject (x : xs) =
  let object = unwrap_ x
   in if objectType predicateObject == objectType object
        then Just x
        else matchingEntry_ predicateObject xs

removeDynamicObject :: (Position -> GameObjectType -> [GameObject]) -> GameObject -> DynamicObjectCollection -> DynamicObjectCollection
removeDynamicObject staticObjectAt object collection =
  let matchingStaticObjects =
        staticObjectAt (getPosition object) (objectType object)
          |> filter ((==) (id object) . id)
   in let updated =
            alter
              (removeObjectFromChunkMap_ object matchingStaticObjects)
              (objChunkKey_ object)
              (elements_ collection)
       in collection {elements_ = updated}

removeObjectFromChunkMap_ :: GameObject -> [GameObject] -> Maybe (IntMap [DynamicObject]) -> Maybe (IntMap [DynamicObject])
removeObjectFromChunkMap_ _ [] Nothing = Nothing
removeObjectFromChunkMap_ object (matchingStaticObject : _) Nothing =
  Just $
    insert (key_ object) [Removed object] empty
removeObjectFromChunkMap_ object staticObjects (Just chunkMap) =
  Just $
    alter
      (removeFromTileObjects_ object staticObjects)
      (key_ object)
      chunkMap

removeFromTileObjects_ :: GameObject -> [GameObject] -> Maybe [DynamicObject] -> Maybe [DynamicObject]
removeFromTileObjects_ _ [] Nothing = Nothing
removeFromTileObjects_ removed (matchingStaticObject : _) Nothing = Just [Removed removed]
removeFromTileObjects_ removed staticObjects (Just tileObjects) =
  case (matchingEntry_ removed tileObjects, staticObjects) of
    (Nothing, []) -> Just tileObjects
    (Nothing, _) -> Just $ Removed removed : tileObjects
    (Just existing, []) -> case existing of
      Added _ -> Just (tileObjects |> filter ((/=) removed . unwrap_))
      Removed _ -> Just tileObjects
    (Just existing, matchingStaticObject : _) -> case existing of
      Added _ ->
        Just
          ( tileObjects
              |> filter ((/=) removed . unwrap_)
              |> filter ((/=) matchingStaticObject . unwrap_)
          )
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
