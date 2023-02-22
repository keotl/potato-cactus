module PotatoCactus.Game.Item where

data Item = Item
  { id :: Int,
    name :: String,
    stackable :: Bool
    -- TODO - complete  - keotl 2023-02-05
  }
  deriving (Show)
