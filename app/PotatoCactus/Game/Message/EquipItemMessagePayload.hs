module PotatoCactus.Game.Message.EquipItemMessagePayload where

data EquipItemMessagePayload = EquipItemMessagePayload
  { itemId :: Int,
    itemIndex :: Int,
    widgetId :: Int
  }
  deriving (Show)
