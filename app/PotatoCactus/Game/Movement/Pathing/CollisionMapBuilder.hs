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
    0 -> markFlatWall (getPosition obj) (facingDirection obj) collisionMap
    10 -> markSolidOccupant (getPosition obj, dimensions_ obj, facingDirection obj) collisionMap
    _ -> collisionMap

dimensions_ :: GameObject -> (Int, Int)
dimensions_ obj =
  case objectDefinition (GameObject.id obj) of
    Nothing -> (1, 1)
    Just def -> (ObjDef.sizeX def, ObjDef.sizeY def)
