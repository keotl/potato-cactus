module PotatoCactus.Game.Definitions.Types.GameObjectDefinition where

type GameObjectId = Int
-- type GameObjectType = Int

data GameObjectDefinition = GameObjectDefinition
  { id :: GameObjectId
  -- objectType :: GameObjectType
  }
  deriving (Show)
