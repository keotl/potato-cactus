{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.GameEventMapper (mapEvent) where

import Data.Aeson (ToJSON, Value (Null))
import Data.Aeson.Text (encodeToTextBuilder)
import GHC.Generics (Generic)
import PotatoCactus.Game.Scripting.Bridge.BridgeMessage (BridgeMessage, EmptyPayload, bridgeMessage)
import PotatoCactus.Game.Scripting.Bridge.ControlMessages (doneSendingEventsMessage)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.InteractionDto (playerInteractionToDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.NpcAttackDto (npcAttackDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.NpcReferenceDto (npcReferenceDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.PlayerAttackDto (playerAttackToDto)
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (NpcAttackEvent, NpcCannotReachTargetEvent, NpcDeadEvent, NpcEntityTickEvent, PlayerAttackEvent, PlayerInteractionEvent))

mapEvent :: GameEvent -> BridgeMessage (GameEventDto Value)
mapEvent (PlayerInteractionEvent p i) =
  bridgeMessage "gameEvent" $
    GameEventDto "playerInteraction" (playerInteractionToDto p i)
mapEvent (PlayerAttackEvent p t) =
  bridgeMessage "gameEvent" $
    GameEventDto "playerAttack" (playerAttackToDto p t)
mapEvent (NpcAttackEvent npc t) =
  bridgeMessage "gameEvent" $
    GameEventDto "npcAttack" (npcAttackDto npc t)
mapEvent (NpcCannotReachTargetEvent npc t) =
  bridgeMessage "gameEvent" $
    GameEventDto "noop" Null
mapEvent (NpcDeadEvent npc) =
  bridgeMessage "gameEvent" $
    GameEventDto "npcDead" (npcReferenceDto npc)
mapEvent (NpcEntityTickEvent npc) =
  bridgeMessage "gameEvent" $
    GameEventDto "npcEntityTick" (npcReferenceDto npc)

data GameEventDto b = GameEventDto
  { event :: String,
    body :: b
  }
  deriving (Show, Generic)

instance (ToJSON b) => ToJSON (GameEventDto b)
