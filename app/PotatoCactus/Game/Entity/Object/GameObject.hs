module PotatoCactus.Game.Entity.Object.GameObject where

import PotatoCactus.Game.Definitions.GameObjectDefinitions (GameObjectId)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position)

data GameObject = GameObject
  { id :: GameObjectId,
    position :: Position
  }
  deriving (Show)

instance GetPosition GameObject where
  getPosition = position
