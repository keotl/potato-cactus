module PotatoCactus.Game.Entity.GroundItem.GroundItem where

import PotatoCactus.Game.Definitions.ItemDefinitions (ItemId)
import qualified PotatoCactus.Game.ItemContainer as ItemStack
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

type GroundItemKey = (ItemId, Int, Position)

matches :: GroundItemKey -> GroundItem -> Bool
matches (keyItemId, keyQuantity, keyPosition) item =
  itemId item == keyItemId
    && quantity item == keyQuantity
    && position item == keyPosition

isExpired :: Int -> GroundItem -> Bool
isExpired time i =
  time >= despawnTime i

toItemStack :: GroundItem -> ItemStack.ItemStack
toItemStack item =
  ItemStack.ItemStack (itemId item) (quantity item)
