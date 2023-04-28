{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.NpcDto (NpcDto, npcDto) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import qualified PotatoCactus.Game.Entity.Npc.Npc as N
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.CombatDto (CombatDto, combatDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.MovementDto (MovementDto, npcMovementDto)

data NpcDto = NpcDto
  { serverIndex :: Int,
    definitionId :: Int,
    movement :: MovementDto,
    combat :: CombatDto
  }
  deriving (Show, Generic)

instance ToJSON NpcDto

npcDto :: N.Npc -> NpcDto
npcDto npc =
  NpcDto
    { serverIndex = N.serverIndex npc,
      definitionId = N.definitionId npc,
      movement = npcMovementDto . N.movement $ npc,
      combat = combatDto . N.combat $ npc
    }
