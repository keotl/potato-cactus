{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.ItemContainerDto (itemStackDto, itemContainerDto) where

import Data.Aeson (Value (Array, Null), object, (.=))
import PotatoCactus.Game.ItemContainer (ItemContainer (ItemContainer, content), ItemStack (Empty, ItemStack))

itemStackDto :: ItemStack -> Value
itemStackDto Empty = Null
itemStackDto (ItemStack itemId quantity) =
  object
    [ "itemId" .= itemId,
      "quantity" .= quantity
    ]

itemContainerDto :: ItemContainer -> [Value]
itemContainerDto ItemContainer {content} =
  map itemStackDto content
