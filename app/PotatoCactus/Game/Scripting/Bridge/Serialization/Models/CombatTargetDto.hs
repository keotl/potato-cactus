{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.CombatTargetDto (combatTargetToDto) where

import Data.Aeson (Value (String, Null), object, (.=))
import PotatoCactus.Game.Combat.CombatEntity (CombatTarget (None, NpcTarget, PlayerTarget))

combatTargetToDto :: CombatTarget -> Value
combatTargetToDto (PlayerTarget playerId) =
  object
    [ "targetType" .= String "player",
      "playerIndex" .= playerId
    ]
combatTargetToDto (NpcTarget npcId) =
  object
    [ "targetType" .= String "npc",
      "npcIndex" .= npcId
    ]
combatTargetToDto None = Null
