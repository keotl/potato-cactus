module PotatoCactus.Game.Entity.Object.GameObjectKey where

import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectId)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position))

data GameObjectKey = GameObjectKey
  { id :: GameObjectId,
    position :: Position
  }
  deriving (Show, Eq)

instance GetPosition GameObjectKey where
  getPosition = position
