{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.GameEventMapper (mapEvent) where

import Data.Aeson (ToJSON, Value (Null))
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Aeson.Types (listValue)
import GHC.ExecutionStack (Location (functionName))
import GHC.Generics (Generic)
import PotatoCactus.Game.Scripting.Actions.ScriptInvocation (ScriptInvocation (ScriptInvocation))
import PotatoCactus.Game.Scripting.Bridge.BridgeMessage (BridgeMessage, EmptyPayload, bridgeMessage)
import PotatoCactus.Game.Scripting.Bridge.ControlMessages (doneSendingEventsMessage)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.CommandDto (commandDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.DropItemDto (dropItemDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.InteractionDto (playerInteractionToDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.NpcAttackDto (npcAttackDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.NpcReferenceDto (npcReferenceDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.PlayerAttackDto (playerAttackToDto)
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent (..))

mapEvent :: GameEvent -> BridgeMessage (GameEventDto Value)
mapEvent ServerInitEvent =
  bridgeMessage "gameEvent" $
    GameEventDto "ServerInitEvent" Null
mapEvent (PlayerInteractionEvent p i) =
  let (eventName, body) = playerInteractionToDto p i
   in bridgeMessage "gameEvent" $ GameEventDto eventName body
mapEvent (PlayerAttackEvent p t) =
  bridgeMessage "gameEvent" $
    GameEventDto "PlayerAttackEvent" (playerAttackToDto p t)
mapEvent (NpcAttackEvent npc t) =
  bridgeMessage "gameEvent" $
    GameEventDto "NpcAttackEvent" (npcAttackDto npc t)
mapEvent (InternalNpcCannotReachTargetEvent npc t) =
  bridgeMessage "gameEvent" $
    GameEventDto "noop" Null
mapEvent (NpcDeadEvent npc) =
  bridgeMessage "gameEvent" $
    GameEventDto "NpcDeadEvent" (npcReferenceDto npc)
mapEvent (NpcEntityTickEvent npc) =
  bridgeMessage "gameEvent" $
    GameEventDto "NpcEntityTickEvent" (npcReferenceDto npc)
mapEvent (PlayerCommandEvent playerIndex cmd args) =
  bridgeMessage "gameEvent" $ GameEventDto "PlayerCommandEvent" (commandDto playerIndex cmd args)
mapEvent (DropItemEvent playerId widgetId itemId index) =
  bridgeMessage "gameEvent" $ GameEventDto "DropItemEvent" (dropItemDto playerId widgetId itemId index)
mapEvent (ScriptInvokedEvent (ScriptInvocation functionName args)) =
  bridgeMessage "invokeScript" $ GameEventDto functionName (listValue id args)
mapEvent (InternalPlayerInteractionPendingPathingEvent _ _) =
  bridgeMessage "gameEvent" $ GameEventDto "noop" Null

data GameEventDto b = GameEventDto
  { event :: String,
    body :: b
  }
  deriving (Show, Generic)

instance (ToJSON b) => ToJSON (GameEventDto b)
