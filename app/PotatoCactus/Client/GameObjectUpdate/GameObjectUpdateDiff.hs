module PotatoCactus.Client.GameObjectUpdate.GameObjectUpdateDiff (computeDiff, GameObjectDiff (Added, Retained, Removed)) where

import Data.List (find)
import Data.Maybe (mapMaybe)
import PotatoCactus.Game.Entity.Object.DynamicObject (DynamicObject)
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as Object
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (objectType))
import PotatoCactus.Game.Position (getPosition)
import Prelude hiding (id)

data GameObjectDiff = Added DynamicObject | Retained DynamicObject | Removed DynamicObject deriving (Eq, Show)

computeDiff :: [DynamicObject] -> [DynamicObject] -> [GameObjectDiff]
computeDiff old new =
  map (mapNewObject old) new
    ++ mapMaybe (mapOldObject new) old

mapNewObject :: [DynamicObject] -> DynamicObject -> GameObjectDiff
mapNewObject oldSet object =
  if object `elem` oldSet
    then Retained object
    else Added object

mapOldObject :: [DynamicObject] -> DynamicObject -> Maybe GameObjectDiff
mapOldObject newSet object =
  if object `elem` newSet
    then Nothing
    else Just $ Removed object
