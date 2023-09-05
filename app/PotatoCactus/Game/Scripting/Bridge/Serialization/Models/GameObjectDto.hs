{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.GameObjectDto (GameObjectDto, dynamicGameObjectToDto, gameObjectToDto) where

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

dynamicGameObjectToDto :: D.DynamicObject -> Maybe GameObjectDto
dynamicGameObjectToDto (D.Added obj) =
  Just $ gameObjectToDto obj
-- TODO - Send Removed/Replacing objects to the script engine  - keotl 2023-08-31
-- TODO - Worth considering as part of a global static/dynamic object strategy  - keotl 2023-08-31
dynamicGameObjectToDto _ = Nothing

gameObjectToDto :: GameObject -> GameObjectDto
gameObjectToDto obj =
  GameObjectDto
    { id = O.id obj,
      position = toDto . O.position $ obj,
      objectType = O.objectType obj,
      facingDirection = O.facingDirection obj
    }
