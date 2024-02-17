{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.MovementDto (MovementDto, npcMovementDto, playerMovementDto) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import qualified PotatoCactus.Game.Entity.Npc.NpcMovement as N
import qualified PotatoCactus.Game.Movement.PlayerMovement as P
import PotatoCactus.Game.Position (getPosition)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.PositionDto (PositionDto (PositionDto), toDto)

data MovementDto = MovementDto
  { position :: PositionDto,
    queue :: [PositionDto],
    isRunning :: Bool
  }
  deriving (Show, Generic)

instance ToJSON MovementDto

npcMovementDto :: N.NpcMovement -> MovementDto
npcMovementDto n =
  MovementDto
    { position = toDto . getPosition $ n,
      queue = map toDto $ N.queue_ n,
      isRunning = False
    }

playerMovementDto :: P.PlayerMovement -> MovementDto
playerMovementDto m =
  MovementDto
    { position = toDto . getPosition $ m,
      queue = map toDto $ P.queue_ m,
      isRunning = P.isRunning m
    }
