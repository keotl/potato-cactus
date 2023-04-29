{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.WorldDto (WorldDto, worldToDto) where

import Data.Aeson (ToJSON)
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as O
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.GameObjectDto (GameObjectDto, gameObjectToDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.NpcDto (NpcDto, npcDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.PlayerDto (PlayerDto, playerDto)
import qualified PotatoCactus.Game.World as W
import PotatoCactus.Game.World.MobList (iter, serialize)

data WorldDto = WorldDto
  { tick :: Int,
    players :: [Maybe PlayerDto],
    npcs :: [Maybe NpcDto],
    objects :: [GameObjectDto]
  }
  deriving (Show, Generic)

instance ToJSON WorldDto

worldToDto :: W.World -> WorldDto
worldToDto w =
  WorldDto
    { tick = W.tick w,
      players = fmap playerDto <$> (serialize . W.players $ w),
      npcs = fmap npcDto <$> (serialize . W.npcs $ w),
      objects = mapMaybe gameObjectToDto (O.iter . W.objects $ w)
    }
