module PotatoCactus.Game.Entity.Object.GameObject where

import PotatoCactus.Game.Definitions.GameObjectDefinitions (GameObjectId)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position)

type GameObjectType = Int

data GameObject = GameObject
  { id :: GameObjectId,
    position :: Position,
    objectType :: GameObjectType,
    facingDirection :: Int
  }
  deriving (Show, Eq)

instance GetPosition GameObject where
  getPosition = position
