module PotatoCactus.Game.Definitions.Types.ItemDefinition where

type ItemId = Int

data ItemDefinition = ItemDefinition
  { id :: ItemId,
    name :: String,
    stackable :: Bool
    -- weight :: Int
    -- equipSlot :: Int, -- -1 if not equipable
    -- equippedShouldHideModel_ :: Bool
  }
