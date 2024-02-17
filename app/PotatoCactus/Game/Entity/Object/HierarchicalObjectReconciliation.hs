module PotatoCactus.Game.Entity.Object.HierarchicalObjectReconciliation (reconcileObjects) where

import Data.IntMap ((!?))
import qualified Data.IntMap as IntMap
import Data.Maybe (catMaybes, mapMaybe)
import PotatoCactus.Game.Entity.Object.DynamicObject (DynamicObject (Added, Removed, Replacing))
import qualified PotatoCactus.Game.Entity.Object.DynamicObject as DynamicObject
import PotatoCactus.Game.Entity.Object.GameObject (GameObject, hashObject)

reconcileObjects :: [DynamicObject] -> [GameObject] -> [GameObject]
reconcileObjects dynamicObjects staticObjects =
  let objectOverrides = dynamicObjectLookup_ dynamicObjects
   in catMaybes
        ( [ pickObjectOnTile_
              (objectOverrides !? hashObject staticObj)
              staticObj
            | staticObj <- staticObjects
          ]
            ++ map onlyAdded_ dynamicObjects
        )

pickObjectOnTile_ :: Maybe (Maybe GameObject) -> GameObject -> Maybe GameObject
pickObjectOnTile_ Nothing staticObject = Just staticObject
pickObjectOnTile_ (Just Nothing) staticObject = Nothing
pickObjectOnTile_ (Just (Just dynamicObject)) _ = Just dynamicObject

onlyAdded_ :: DynamicObject -> Maybe GameObject
onlyAdded_ (DynamicObject.Added obj) = Just obj
onlyAdded_ _ = Nothing

dynamicObjectLookup_ :: [DynamicObject] -> IntMap.IntMap (Maybe GameObject)
dynamicObjectLookup_ =
  IntMap.fromList . mapMaybe oldObjectEntry_

oldObjectEntry_ :: DynamicObject -> Maybe (Int, Maybe GameObject)
oldObjectEntry_ (Added _) = Nothing
oldObjectEntry_ (Replacing newObj oldObj) = Just (hashObject oldObj, Just newObj)
oldObjectEntry_ (Removed oldObj) = Just (hashObject oldObj, Nothing)
