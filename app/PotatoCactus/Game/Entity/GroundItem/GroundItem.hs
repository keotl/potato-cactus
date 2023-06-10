module PotatoCactus.Game.Entity.GroundItem.GroundItem where

import PotatoCactus.Game.Definitions.ItemDefinitions (ItemId)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position)

data GroundItem = GroundItem
  { itemId :: ItemId,
    quantity :: Int,
    position :: Position,
    player :: Maybe String,
    despawnTime :: Int
  }
  deriving (Show)

instance GetPosition GroundItem where
  getPosition = position

instance Eq GroundItem where
  x == y = (itemId x, quantity x, position x) == (itemId y, quantity y, position y)

isExpired :: Int -> GroundItem -> Bool
isExpired time i =
  time >= despawnTime i
