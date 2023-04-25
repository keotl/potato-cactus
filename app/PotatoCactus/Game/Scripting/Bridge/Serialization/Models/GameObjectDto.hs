{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.GameObjectDto (GameObjectDto, gameObjectToDto) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import qualified PotatoCactus.Game.Entity.Object.GameObject as O
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.PositionDto (PositionDto, toDto)
import Prelude hiding (id)

data GameObjectDto = GameObjectDto
  { id :: Int,
    position :: PositionDto,
    objectType :: Int,
    facingDirection :: Int
  }
  deriving (Show, Generic)

instance ToJSON GameObjectDto

gameObjectToDto :: O.GameObject -> GameObjectDto
gameObjectToDto obj =
  GameObjectDto
    { id = O.id obj,
      position = toDto . O.position $ obj,
      objectType = O.objectType obj,
      facingDirection = O.facingDirection obj
    }
