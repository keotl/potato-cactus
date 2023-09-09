module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.VarpsetDto (varpsetDto) where

import Data.Aeson (ToJSONKey (toJSONKey), Value, (.=))
import Data.Aeson.Key (Key, fromString)
import Data.Aeson.Types (object)
import Data.Binary (Word32)
import qualified PotatoCactus.Game.PlayerUpdate.VarpSet as VarpSet

varpsetDto :: VarpSet.VarpSet -> Value
varpsetDto varpset =
  object $
    map
      ( \varp ->
          key_ varp .= VarpSet.value varp
      )
      (VarpSet.allValues varpset)

key_ :: VarpSet.Varp -> Key
key_ = fromString . show . VarpSet.varpId
