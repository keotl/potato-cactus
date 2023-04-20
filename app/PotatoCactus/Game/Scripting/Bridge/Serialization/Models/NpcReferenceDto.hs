{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.NpcReferenceDto (npcReferenceDto) where

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Value)
import PotatoCactus.Game.Entity.Npc.Npc (Npc (serverIndex))

npcReferenceDto :: Npc -> Value
npcReferenceDto npc =
  object
    [ "npcIndex" .= serverIndex npc
    ]
