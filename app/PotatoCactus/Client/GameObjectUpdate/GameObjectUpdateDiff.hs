module PotatoCactus.Client.GameObjectUpdate.GameObjectUpdateDiff (computeDiff, GameObjectDiff (Added, Retained, Removed)) where

import Data.List (find)
import Data.Maybe (mapMaybe)
import PotatoCactus.Game.Definitions.GameObjectDefinitions (GameObjectDefinition (objectType), objectDefinition)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject)
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as Object
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (id))
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
  if not $ hasOtherInSameSpot object newSet
    then Just $ Removed object
    else Nothing

hasOtherInSameSpot :: DynamicObject -> [DynamicObject] -> Bool
hasOtherInSameSpot (Object.Added obj) set =
  any
    ( ( \other ->
          getPosition other == getPosition obj
            && objectType_ obj == objectType_ other
      )
        . unwrap_
    )
    set
hasOtherInSameSpot (Object.Removed _) _ =
  False

objectType_ :: GameObject -> Int
objectType_ obj =
  maybe 10 objectType (objectDefinition . id $ obj)

unwrap_ :: DynamicObject -> GameObject
unwrap_ (Object.Added obj) = obj
unwrap_ (Object.Removed obj) = obj
