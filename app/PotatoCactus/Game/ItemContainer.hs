module PotatoCactus.Game.ItemContainer where

import Data.List (findIndex)
import PotatoCactus.Game.Item (Item (id, stackable))
import PotatoCactus.Game.Typing (Advance (advance))
import PotatoCactus.Utils.Iterable (replaceAtIndex)
import Prelude hiding (id)

data StackPolicy = Always | Standard | Never deriving (Enum, Show)

data ItemStack
  = ItemStack
      { item :: Item,
        quantity :: Int
      }
  | Empty
  deriving (Show)

data ItemContainer = ItemContainer
  { capacity :: Int,
    stackPolicy :: StackPolicy,
    widgetId :: Int,
    content :: [ItemStack],
    willUpdate_ :: Bool, -- Whether the update flag should be set on the next tick cycle
    updated :: Bool -- Whether to send update to player on the current tick
  }

instance Show ItemContainer where
  show = show . content

instance Advance ItemContainer where
  advance container = container {willUpdate_ = False, updated = willUpdate_ container}

playerInventory :: ItemContainer
playerInventory =
  ItemContainer
    { capacity = 28,
      stackPolicy = Standard,
      widgetId = 3214,
      content = replicate 28 Empty,
      willUpdate_ = True,
      updated = False
    }

playerEquipmentContainer :: ItemContainer
playerEquipmentContainer =
  ItemContainer
    { capacity = 14,
      stackPolicy = Standard,
      widgetId = 1688,
      content = replicate 14 Empty,
      willUpdate_ = True,
      updated = False
    }

stacksWith :: StackPolicy -> ItemStack -> ItemStack -> Bool
stacksWith _ Empty _ = True
stacksWith _ _ Empty = True
stacksWith Always (ItemStack a _) (ItemStack b _) = id a == id b
stacksWith Never _ _ = False
stacksWith Standard (ItemStack a _) (ItemStack b _) = (id a == id b) && stackable a

canAddItem :: ItemContainer -> ItemStack -> Bool
canAddItem container stack =
  any (stacksWith (stackPolicy container) stack) (content container)

canAddItems :: ItemContainer -> [ItemStack] -> Bool
canAddItems container = all (canAddItem container)

combine_ :: ItemStack -> ItemStack -> ItemStack
combine_ Empty b = b
combine_ a Empty = a
combine_ a b = a {quantity = quantity a + quantity b}

addItem :: ItemContainer -> ItemStack -> ItemContainer
addItem container item =
  case findIndex (stacksWith (stackPolicy container) item) (content container) of
    Just i ->
      container
        { content =
            replaceAtIndex
              i
              (combine_ (content container !! i) item)
              (content container),
          willUpdate_ = True
        }
    Nothing -> container

addItems :: ItemContainer -> [ItemStack] -> ItemContainer
addItems = foldl addItem

replaceStack :: Int -> ItemContainer -> ItemStack -> (ItemContainer, ItemStack)
replaceStack index container item =
  let old = content container !! index
   in ( container
          { content =
              replaceAtIndex
                index
                item
                (content container),
            willUpdate_ = True
          },
        old
      )

atIndex :: Int -> ItemContainer -> ItemStack
atIndex index container =
  content container !! index
