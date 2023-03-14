module PotatoCactus.Game.Entity.Object.DynamicObjectCollection where

import Data.IntMap (IntMap, delete, empty, toList)
import Data.IntMap.Lazy (insert)
import PotatoCactus.Game.Definitions.GameObjectDefinitions (GameObjectDefinition (objectType), GameObjectType, objectDefinition)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (id, position))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position, x, y, z), chunkX, chunkY)
import Prelude hiding (id)

data DynamicObject = Added GameObject | Removed GameObject deriving (Eq, Show)

instance GetPosition DynamicObject where
  getPosition (Added object) = getPosition object
  getPosition (Removed object) = getPosition object

data DynamicObjectCollection = DynamicObjectCollection
  { elements_ :: IntMap DynamicObject
  }
  deriving (Show)

create :: DynamicObjectCollection
create = DynamicObjectCollection empty

addDynamicObject :: DynamicObject -> DynamicObjectCollection -> DynamicObjectCollection
addDynamicObject object collection =
  let updatedElements = insert (key_ object) object (elements_ collection)
   in collection {elements_ = updatedElements}

removeDynamicObject :: (Position, GameObjectType) -> DynamicObjectCollection -> DynamicObjectCollection
removeDynamicObject keyLike collection =
  let updatedElements = delete (rawKey_ keyLike) (elements_ collection)
   in collection {elements_ = updatedElements}

key_ :: DynamicObject -> Int
key_ (Added object) = rawKey_ (position object, objectTypeKey_ . objectDefinition . id $ object)
key_ (Removed object) = rawKey_ (position object, objectTypeKey_ . objectDefinition . id $ object)


rawKey_ :: (Position, GameObjectType) -> Int
rawKey_ (position, objectType) =
  x position
    + y position * 10 ^ 5
    + z position * 10 ^ 10
    + objectType * 10 ^ 11

objectTypeKey_ :: Maybe GameObjectDefinition -> Int
objectTypeKey_ Nothing = 10
objectTypeKey_ (Just def) = objectType def

-- TODO - use a more suitable data structure  - keotl 2023-03-12
findByChunkXY :: Int -> Int -> DynamicObjectCollection -> [DynamicObject]
findByChunkXY x y collection =
  filter
    ( \object ->
        ( chunkX . getPosition $ object,
          chunkY . getPosition $ object
        )
          == (x, y)
    )
    (map snd $ toList . elements_ $ collection)
