module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.EntityDataDto where

import Data.Aeson (Object, Value (Object), (.=))
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (object)
import qualified Data.Map as Map
import PotatoCactus.Game.Entity.EntityData (EntityData)

entityDataDto :: EntityData -> Value
entityDataDto d =
  object $ map (\(k, v) -> fromString k .= v) (Map.assocs d)
