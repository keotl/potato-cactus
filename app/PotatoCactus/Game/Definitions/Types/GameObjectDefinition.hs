module PotatoCactus.Game.Definitions.Types.GameObjectDefinition where

type GameObjectId = Int

-- type GameObjectType = Int

data GameObjectDefinition = GameObjectDefinition
  { id :: GameObjectId,
    sizeX :: Int,
    sizeY :: Int,
    wall :: Bool,
    walkable :: Bool,
    solid :: Bool,
    face :: Int
    -- objectType :: GameObjectType
  }
  deriving (Show)
