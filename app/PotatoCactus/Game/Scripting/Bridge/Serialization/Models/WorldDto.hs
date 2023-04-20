{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.WorldDto (WorldDto, worldToDto) where

import Data.Aeson (ToJSON)
import Data.IntMap (IntMap, empty)
import GHC.Generics (Generic)
import qualified PotatoCactus.Game.World as W

data WorldDto = WorldDto
  { tick :: Int,
    players :: IntMap PlayerDto
  }
  deriving (Show, Generic)

instance ToJSON WorldDto

data PlayerDto = PlayerDto
  { serverIndex :: Int,
    username :: String
  }
  deriving (Show, Generic)

instance ToJSON PlayerDto

worldToDto :: W.World -> WorldDto
worldToDto w =
  WorldDto
    { tick = W.tick w,
      players = empty
    }
