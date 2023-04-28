{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.PlayerAttackDto (playerAttackToDto) where

import Data.Aeson (Value, (.=))
import Data.Aeson.Types (object)
import PotatoCactus.Game.Combat.CombatEntity (CombatTarget)
import PotatoCactus.Game.Player (Player (serverIndex))
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.CombatTargetDto (combatTargetToDto)

playerAttackToDto :: Player -> CombatTarget -> Value
playerAttackToDto p target =
  object
    [ "playerIndex" .= serverIndex p,
      "target" .= combatTargetToDto target
    ]
