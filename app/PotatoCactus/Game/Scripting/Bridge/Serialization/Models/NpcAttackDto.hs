{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.NpcAttackDto (npcAttackDto) where

import Data.Aeson (Value, (.=))
import Data.Aeson.Types (object)
import PotatoCactus.Game.Combat.CombatEntity (CombatTarget)
import PotatoCactus.Game.Entity.Npc.Npc (Npc (serverIndex))
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.CombatTargetDto (combatTargetToDto)

npcAttackDto :: Npc -> CombatTarget -> Value
npcAttackDto npc t =
  object
    [ "npcIndex" .= serverIndex npc,
      "target" .= combatTargetToDto t
    ]
