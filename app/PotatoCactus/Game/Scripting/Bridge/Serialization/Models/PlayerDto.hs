{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.PlayerDto (PlayerDto, playerDto) where

import Data.Aeson (Object, ToJSON, Value)
import GHC.Generics (Generic)
import qualified PotatoCactus.Game.Player as P
import qualified PotatoCactus.Game.PlayerUpdate.Equipment as EQ
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.CombatDto (CombatDto, combatDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.EntityDataDto (entityDataDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.InteractionDto (interactionToDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.ItemContainerDto (itemContainerDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.MovementDto (MovementDto, playerMovementDto)

data PlayerDto = PlayerDto
  { serverIndex :: Int,
    username :: String,
    movement :: MovementDto,
    combat :: CombatDto,
    interaction :: Value,
    inventory :: [Value],
    equipment :: [Value],
    entityData :: Value
  }
  deriving (Show, Generic)

instance ToJSON PlayerDto

playerDto :: P.Player -> PlayerDto
playerDto p =
  PlayerDto
    { serverIndex = P.serverIndex p,
      username = P.username p,
      movement = playerMovementDto . P.movement $ p,
      combat = combatDto . P.combat $ p,
      interaction = interactionToDto . P.interaction $ p,
      inventory = itemContainerDto . P.inventory $ p,
      equipment = itemContainerDto . EQ.container . P.equipment $ p,
      entityData = entityDataDto . P.entityData $ p
    }
