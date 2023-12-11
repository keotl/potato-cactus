module PotatoCactus.Game.Movement.Pathing.CollisionMapBuilder (buildCollisionMap, recomputeDirtyRegions) where

import PotatoCactus.Game.Definitions.GameObjectDefinitions (objectDefinition)
import qualified PotatoCactus.Game.Definitions.Types.GameObjectDefinition as ObjDef
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject, facingDirection))
import qualified PotatoCactus.Game.Entity.Object.GameObject as GameObject
import PotatoCactus.Game.Movement.Pathing.CollisionMap (CollisionMap, alterForDirtyRegions, markFlatWall, markSolidOccupant)
import qualified PotatoCactus.Game.Movement.Pathing.CollisionMap as CollisionMap
import PotatoCactus.Game.Position (getPosition)

buildCollisionMap :: [GameObject] -> CollisionMap
buildCollisionMap = flip addObjectsToCollisionMap CollisionMap.create

addObjectsToCollisionMap :: [GameObject] -> CollisionMap -> CollisionMap
addObjectsToCollisionMap objects collisionMap = foldl processObject_ collisionMap objects

recomputeDirtyRegions :: (Int -> [GameObject]) -> CollisionMap -> CollisionMap
recomputeDirtyRegions getRegionObjects =
  alterForDirtyRegions
    ( \dirtyRegions startValue ->
        foldl processObject_ startValue (concatMap getRegionObjects dirtyRegions)
    )

processObject_ :: CollisionMap -> GameObject -> CollisionMap
processObject_ collisionMap obj =
  case GameObject.objectType obj of
    0 -> processFlatWall_ collisionMap obj
    10 -> processInteractable_ collisionMap obj
    _ -> collisionMap -- TODO - add other object types, e.g. wall corners  - keotl 2023-11-03

processFlatWall_ :: CollisionMap -> GameObject -> CollisionMap
processFlatWall_ collisionMap obj =
  case objectDefinition (GameObject.id obj) of
    Nothing -> collisionMap
    Just def ->
      if ObjDef.wall def -- Should we also check for solid?
        then markFlatWall (getPosition obj) (facingDirection obj) collisionMap
        else collisionMap

processInteractable_ :: CollisionMap -> GameObject -> CollisionMap
processInteractable_ collisionMap obj =
  case objectDefinition (GameObject.id obj) of
    Nothing ->
      -- Assume solid 1x1 object with actions when no definition is found to aid unit testing.
      markSolidOccupant
        (getPosition obj, (1, 1), facingDirection obj)
        collisionMap
    Just def ->
      if ObjDef.solid def && ObjDef.hasActions def
        then
          markSolidOccupant
            (getPosition obj, (ObjDef.sizeX def, ObjDef.sizeY def), facingDirection obj)
            collisionMap
        else collisionMap
