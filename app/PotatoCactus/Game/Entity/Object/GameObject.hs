module PotatoCactus.Game.Entity.Object.GameObject where

import PotatoCactus.Config.Constants (positionBoundExponentXY_, positionBoundExponentZ_)
import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectId)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position))
import qualified PotatoCactus.Game.Position as Pos
import PotatoCactus.Game.Typing (ToNumeric (toNumeric))

type GameObjectType = Int

-- 0 -> Regular wall
-- 1 -> Triangle wall column
-- 2 -> Wall corner (L-shape)
-- 3 -> Square wall column
-- 4 -> Wall decoration (no interaction?)
-- 5 -> Wall decoration (with interaction?)
-- 9 -> Diagonal wall (tile fully blocked)
-- 10 -> Standard game object (i.e. interactable)

data GameObject = GameObject
  { id :: GameObjectId,
    position :: Position,
    objectType :: GameObjectType,
    facingDirection :: Int
  }
  deriving (Show, Eq)

instance GetPosition GameObject where
  getPosition = position

data GameObjectTypeGroup -- Up to one of each on a single tile
  = WallObject
  | WallDecoration
  | InteractiveObject -- Up to 5 of these on a single tile
  | GroundDecoration

objectTypeGroup :: GameObjectType -> GameObjectTypeGroup
objectTypeGroup objType
  | objType <= 3 = WallObject
  | objType <= 8 = WallDecoration
  | objType <= 21 = InteractiveObject
  | otherwise = GroundDecoration

instance ToNumeric GameObjectTypeGroup where
  toNumeric WallObject = 0
  toNumeric WallDecoration = 1
  toNumeric InteractiveObject = 2
  toNumeric GroundDecoration = 3

gameObjectHash :: (Position, GameObjectType) -> Int
gameObjectHash (position, objectType) =
  Pos.x position
    + Pos.y position * 10 ^ keyExponentY_
    + Pos.z position * 10 ^ keyExponentZ_
    + objectType * 10 ^ keyExponentObjType_

hashObject :: GameObject -> Int
hashObject object =
  gameObjectHash (position object, objectType object)

keyExponentY_ = positionBoundExponentXY_

keyExponentZ_ = 2 * positionBoundExponentXY_

keyExponentObjType_ = (2 * positionBoundExponentXY_) + positionBoundExponentZ_
