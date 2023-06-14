module PotatoCactus.Game.Entity.EntityData (EntityData, create, setValue) where

import Data.Aeson (Value)
import qualified Data.Map.Lazy as Map

type EntityData = Map.Map String Value

create :: EntityData
create = Map.empty

setValue :: EntityData -> String -> Value -> EntityData
setValue store key val =
  Map.insert key val store
