module PotatoCactus.Game.ItemContainer where

import PotatoCactus.Game.Item (Item)

data StackPolicy = Always | Standard | Never deriving (Enum)

data ItemStack
  = ItemStack
      { item :: Item,
        quantity :: Int
      }
  | Empty

data ItemContainer = ItemContainer
  { capacity :: Int,
    stackPolicy :: StackPolicy,
    widgetId :: Int,
    content :: [ItemStack]
  }

playerInventory :: ItemContainer
playerInventory =
  ItemContainer
    { capacity = 28,
      stackPolicy = Standard,
      widgetId = 3214,
      content = []
    }

playerEquipment :: ItemContainer
playerEquipment =
  ItemContainer
    { capacity = 14,
      stackPolicy = Standard,
      widgetId = 1688,
      content = []
    }
