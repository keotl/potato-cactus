module PotatoCactus.Game.Movement.Pathing.CollisionMapBuilder (buildCollisionMap) where

import PotatoCactus.Game.Definitions.GameObjectDefinitions (objectDefinition)
import qualified PotatoCactus.Game.Definitions.Types.GameObjectDefinition as ObjDef
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject, facingDirection))
import qualified PotatoCactus.Game.Entity.Object.GameObject as GameObject
import PotatoCactus.Game.Movement.Pathing.CollisionMap (CollisionMap, markFlatWall, markSolidOccupant)
import qualified PotatoCactus.Game.Movement.Pathing.TileFlagsMap as TileFlagsMap
import PotatoCactus.Game.Position (getPosition)

buildCollisionMap :: [GameObject] -> CollisionMap
buildCollisionMap = foldl processObject_ TileFlagsMap.create

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
    Nothing -> collisionMap
    Just def ->
      if ObjDef.solid def && ObjDef.hasActions def
        then
          markSolidOccupant
            (getPosition obj, (ObjDef.sizeX def, ObjDef.sizeY def), facingDirection obj)
            collisionMap
        else collisionMap
