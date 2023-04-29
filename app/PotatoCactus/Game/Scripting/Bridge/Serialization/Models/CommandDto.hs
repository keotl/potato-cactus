{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.CommandDto (commandDto) where

import Data.Aeson (Value, object, (.=))
import PotatoCactus.Game.Player (PlayerIndex)

commandDto :: PlayerIndex -> String -> [String] -> Value
commandDto playerIndex command args =
  object
    [ "playerIndex" .= playerIndex,
      "command" .= command,
      "args" .= args
    ]
