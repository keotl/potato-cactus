{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.WorldDto (WorldDto, worldToDto) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.NpcDto (NpcDto, npcDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.PlayerDto (PlayerDto, playerDto)
import qualified PotatoCactus.Game.World as W
import PotatoCactus.Game.World.MobList (iter)

data WorldDto = WorldDto
  { tick :: Int,
    players :: [PlayerDto],
    npcs :: [NpcDto]
  }
  deriving (Show, Generic)

instance ToJSON WorldDto

worldToDto :: W.World -> WorldDto
worldToDto w =
  WorldDto
    { tick = W.tick w,
      players = map playerDto (iter . W.players $ w),
      npcs = map npcDto (iter . W.npcs $ w)
    }
