{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.CombatDto (CombatDto, combatDto) where

import Data.Aeson (ToJSON, Value)
import GHC.Generics (Generic)
import qualified PotatoCactus.Game.Combat.CombatEntity as C
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.CombatTargetDto (combatTargetToDto)

data CombatDto = CombatDto
  { hitpoints :: Int,
    state :: String,
    maxHitpoints :: Int,
    target :: Value,
    cooldown :: Int
  }
  deriving (Show, Generic)

instance ToJSON CombatDto

combatDto :: C.CombatEntity -> CombatDto
combatDto c =
  CombatDto
    { hitpoints = C.hitpoints c,
      state = mapCombatState . C.state $ c,
      maxHitpoints = C.maxHitpoints c,
      target = combatTargetToDto . C.target $ c,
      cooldown = C.cooldown c
    }

mapCombatState :: C.CombatState -> String
mapCombatState C.Alive = "alive"
mapCombatState C.Dying = "dying"
mapCombatState C.Dead = "dead"
