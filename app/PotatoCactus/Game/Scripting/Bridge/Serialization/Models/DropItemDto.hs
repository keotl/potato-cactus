{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.DropItemDto where

import Data.Aeson (Value, object, (.=))
import PotatoCactus.Game.Definitions.Types.ItemDefinition (ItemId)
import PotatoCactus.Game.Player (PlayerIndex)
import PotatoCactus.Game.Scripting.Actions.CreateInterface (WidgetId)

dropItemDto :: PlayerIndex -> WidgetId -> ItemId -> Int -> Value
dropItemDto playerId widgetId itemId index =
  object
    [ "playerIndex" .= playerId,
      "widgetId" .= widgetId,
      "itemId" .= itemId,
      "index" .= index
    ]
