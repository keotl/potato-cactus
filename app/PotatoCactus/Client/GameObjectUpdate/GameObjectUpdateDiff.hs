module PotatoCactus.Client.GameObjectUpdate.GameObjectUpdateDiff (computeDiff, GameObjectDiff (Added, Retained, Removed)) where

import Data.List (find)
import Data.Maybe (mapMaybe)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject)

data GameObjectDiff = Added DynamicObject | Retained DynamicObject | Removed DynamicObject

computeDiff :: [DynamicObject] -> [DynamicObject] -> [GameObjectDiff]
computeDiff old new =
  map (mapNewObject old) new
    ++ mapMaybe (mapOldObject new) old

mapNewObject :: [DynamicObject] -> DynamicObject -> GameObjectDiff
mapNewObject oldSet object =
  if includes object oldSet
    then Retained object
    else Added object

mapOldObject :: [DynamicObject] -> DynamicObject -> Maybe GameObjectDiff
mapOldObject newSet object =
  if not $ includes object newSet
    then Just $ Removed object
    else Nothing

includes :: DynamicObject -> [DynamicObject] -> Bool
includes obj set =
  case find (== obj) set of
    Nothing -> False
    Just _ -> True
