{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.StaticGameObjectSetDto (StaticGameObjectSetDto, staticGameObjectSetDto) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import PotatoCactus.Game.Definitions.StaticGameObjectSet (StaticGameObjectSet)
import qualified PotatoCactus.Game.Definitions.StaticGameObjectSet as StaticObjects
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.GameObjectDto (GameObjectDto, gameObjectToDto)

data StaticGameObjectSetDto = StaticGameObjectSetDto
  { objects :: [GameObjectDto]
  }
  deriving (Show, Generic)

instance ToJSON StaticGameObjectSetDto

staticGameObjectSetDto :: StaticGameObjectSet -> StaticGameObjectSetDto
staticGameObjectSetDto staticObjects =
  StaticGameObjectSetDto $
    map
      gameObjectToDto
      (StaticObjects.allEntries staticObjects)
