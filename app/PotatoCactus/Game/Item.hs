module PotatoCactus.Game.Item where

import Prelude hiding (id)

data Item = Item
  { id :: Int,
    -- name :: String,
    stackable :: Bool,
    equipSlot :: Int, -- -1 if not equipable
    equippedShouldHideModel_ :: Bool
  }
  deriving (Show)

-- Utility builder functions

simpleItem :: Int -> Item
simpleItem itemId =
  Item
    { id = itemId,
      stackable = False,
      equipSlot = -1,
      equippedShouldHideModel_ = False
    }

stackableItem :: Int -> Item
stackableItem itemId =
  (simpleItem itemId) {stackable = True}

armourItem :: Int -> Int -> Bool -> Item
armourItem itemId equipSlot shouldHideModel =
  Item
    { id = itemId,
      stackable = False,
      equipSlot = equipSlot,
      equippedShouldHideModel_ = shouldHideModel
    }
