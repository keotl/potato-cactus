{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.PositionDto where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import qualified PotatoCactus.Game.Position as P

data PositionDto = PositionDto
  { x :: Int,
    y :: Int,
    z :: Int
  }
  deriving (Show, Generic, ToJSON)

toDto :: P.Position -> PositionDto
toDto (P.Position xx yy zz) =
  PositionDto xx yy zz
