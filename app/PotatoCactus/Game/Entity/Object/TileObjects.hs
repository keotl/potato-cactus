module PotatoCactus.Game.Entity.Object.TileObjects (create, TileObjects, DynamicObject (..), addObject, removeObject, objects) where

import Data.IntMap (IntMap, empty)
import qualified Data.IntMap as IntMap
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (objectType), GameObjectType)

-- Represents an overlay over the static object set at any given time.
-- Static Set + Dynamic Set = Objects as they are visible to clients
data DynamicObject = Added GameObject | Replacing GameObject | Removed GameObjectType deriving (Show, Eq)

type StaticObjectOnTile = Maybe GameObject

type StaticObjectLookup = GameObjectType -> StaticObjectOnTile

data TileObjects = TileObjects
  { objects_ :: IntMap DynamicObject, -- objectType -> Object
    staticObject_ :: StaticObjectLookup
  }

create :: StaticObjectLookup -> TileObjects
create = TileObjects empty

addObject :: GameObject -> TileObjects -> TileObjects
addObject obj collection =
  let updated =
        IntMap.alter
          ( alterEntryForAddition_
              obj
              (staticObject_ collection . objectType $ obj)
          )
          (objectType obj)
          (objects_ collection)
   in collection {objects_ = updated}

alterEntryForAddition_ :: GameObject -> StaticObjectOnTile -> Maybe DynamicObject -> Maybe DynamicObject
alterEntryForAddition_ newObject Nothing _ = Just $ Added newObject
alterEntryForAddition_ newObject (Just staticObj) _ =
  if newObject == staticObj then Nothing else Just $ Replacing newObject

removeObject :: GameObjectType -> TileObjects -> TileObjects
removeObject objType collection =
  let staticObjectOnTile = staticObject_ collection objType
   in let updated =
            IntMap.alter
              (alterEntryForRemoval_ staticObjectOnTile)
              objType
              (objects_ collection)
       in collection {objects_ = updated}

alterEntryForRemoval_ :: StaticObjectOnTile -> Maybe DynamicObject -> Maybe DynamicObject
alterEntryForRemoval_ Nothing _ = Nothing
alterEntryForRemoval_ (Just staticItem) _ = Just $ Removed . objectType $ staticItem

objects :: TileObjects -> [DynamicObject]
objects collection =
  map snd (IntMap.toList (objects_ collection))
