module PotatoCactus.Game.Entity.Object.TileObjects (create, TileObjects, addObject, removeObject, objects, findVisibleObjectById) where

import Data.IntMap (IntMap, empty)
import qualified Data.IntMap as IntMap
import Data.Maybe (listToMaybe)
import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectId)
import PotatoCactus.Game.Entity.Object.DynamicObject (DynamicObject (Added, Removed, Replacing), VisibleObject (Hidden, None, Visible), hasFoundMatch)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (objectType), GameObjectType)
import qualified PotatoCactus.Game.Entity.Object.GameObject as GameObject
import PotatoCactus.Utils.Flow ((|>))

type StaticObjectOnTile = Maybe GameObject

type StaticObjectLookup = GameObjectType -> StaticObjectOnTile

data TileObjects = TileObjects
  { objects_ :: IntMap DynamicObject, -- objectType -> Object
    staticObject_ :: StaticObjectLookup
  }

instance Show TileObjects where
  show = show . objects_

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
  if newObject == staticObj then Nothing else Just $ Replacing newObject staticObj

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
alterEntryForRemoval_ (Just staticItem) _ = Just $ Removed staticItem

objects :: TileObjects -> [DynamicObject]
objects collection =
  map snd (IntMap.toList (objects_ collection))

findVisibleObjectById :: GameObjectId -> TileObjects -> VisibleObject
findVisibleObjectById objectId collection =
  case objects collection
    |> map (matchVisibleObjectById_ objectId)
    |> filter hasFoundMatch of
    [] -> None
    x : xs -> x

matchVisibleObjectById_ :: GameObjectId -> DynamicObject -> VisibleObject
matchVisibleObjectById_ objId (Added object) =
  if GameObject.id object == objId
    then Visible object
    else None
matchVisibleObjectById_ objId (Replacing newObj _) =
  if GameObject.id newObj == objId
    then Visible newObj
    else None
matchVisibleObjectById_ objId (Removed removedObj) =
  if GameObject.id removedObj == objId
    then Hidden
    else None
