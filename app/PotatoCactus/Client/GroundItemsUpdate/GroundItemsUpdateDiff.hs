module PotatoCactus.Client.GroundItemsUpdate.GroundItemsUpdateDiff (fromGroundItem, computeDiff, GroundItemClientView (..), GroundItemDiff (..)) where

import Data.Maybe (mapMaybe)
import PotatoCactus.Game.Definitions.ItemDefinitions (ItemId)
import PotatoCactus.Game.Entity.GroundItem.GroundItem (GroundItem)
import qualified PotatoCactus.Game.Entity.GroundItem.GroundItem as GroundItem
import PotatoCactus.Game.Position (GetPosition (getPosition), Position)

data GroundItemClientView = GroundItemClientView
  { itemId :: ItemId,
    quantity :: Int,
    position :: Position
  }
  deriving (Eq, Show)

instance GetPosition GroundItemClientView where
  getPosition = position

fromGroundItem :: GroundItem -> GroundItemClientView
fromGroundItem i =
  GroundItemClientView
    { itemId = GroundItem.itemId i,
      quantity = GroundItem.quantity i,
      position = GroundItem.position i
    }

data GroundItemDiff = Added GroundItemClientView | Retained GroundItemClientView | Removed GroundItemClientView deriving (Eq, Show)

computeDiff :: [GroundItemClientView] -> [GroundItemClientView] -> [GroundItemDiff]
computeDiff old new =
  map (mapNewObject old) new
    ++ mapMaybe (mapOldObject new) old

mapNewObject :: [GroundItemClientView] -> GroundItemClientView -> GroundItemDiff
mapNewObject oldSet object =
  if object `elem` oldSet
    then Retained object
    else Added object

mapOldObject :: [GroundItemClientView] -> GroundItemClientView -> Maybe GroundItemDiff
mapOldObject newSet object =
  if object `notElem` newSet
    then Just $ Removed object
    else Nothing
