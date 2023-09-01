{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.GameObjectDto (GameObjectDto, gameObjectToDto, objectPlacementToDto) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import qualified PotatoCactus.Game.Entity.Object.DynamicObject as D
import PotatoCactus.Game.Entity.Object.GameObject (GameObject)
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

gameObjectToDto :: D.DynamicObject -> Maybe GameObjectDto
gameObjectToDto (D.Added obj) =
  Just $ objectPlacementToDto obj
-- TODO - Send Removed/Replacing objects to the script engine  - keotl 2023-08-31
-- TODO - Worth considering as part of a global static/dynamic object strategy  - keotl 2023-08-31
gameObjectToDto _ = Nothing

objectPlacementToDto :: GameObject -> GameObjectDto
objectPlacementToDto obj =
  GameObjectDto
    { id = O.id obj,
      position = toDto . O.position $ obj,
      objectType = O.objectType obj,
      facingDirection = O.facingDirection obj
    }
