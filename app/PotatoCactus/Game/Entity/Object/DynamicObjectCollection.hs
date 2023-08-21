module PotatoCactus.Game.Entity.Object.DynamicObjectCollection where

import Data.IntMap (IntMap, delete, empty, toList)
import Data.IntMap.Lazy (insert)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (objectType, position), GameObjectType, gameObjectHash)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position, x, y, z), chunkX, chunkY)
import qualified PotatoCactus.Game.Position as Pos
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
  let updatedElements = delete (gameObjectHash keyLike) (elements_ collection)
   in collection {elements_ = updatedElements}

key_ :: DynamicObject -> Int
key_ (Added object) = gameObjectHash (position object, objectType object)
key_ (Removed object) = gameObjectHash (position object, objectType object)

-- TODO - use a more suitable data structure  - keotl 2023-03-12
findByChunkXY :: Int -> Int -> Int -> DynamicObjectCollection -> [DynamicObject]
findByChunkXY x y z collection =
  filter
    ( \object ->
        ( chunkX . getPosition $ object,
          chunkY . getPosition $ object,
          Pos.z . getPosition $ object
        )
          == (x, y, z)
    )
    (map snd $ toList . elements_ $ collection)

-- For serialization
iter :: DynamicObjectCollection -> [DynamicObject]
iter collection =
  map snd $ toList . elements_ $ collection
