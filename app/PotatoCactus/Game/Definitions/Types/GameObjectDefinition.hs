module PotatoCactus.Game.Definitions.Types.GameObjectDefinition where

type GameObjectId = Int

data GameObjectDefinition = GameObjectDefinition
  { id :: GameObjectId,
    sizeX :: Int,
    sizeY :: Int,
    wall :: Bool,
    walkable :: Bool,
    solid :: Bool,
    face :: Int
  }
  deriving (Show)
