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

mapEvent :: GameEvent -> Maybe (BridgeMessage (GameEventDto Value))
mapEvent ServerInitEvent =
  Just $ bridgeMessage "gameEvent" (GameEventDto "ServerInitEvent" Null)
mapEvent (PlayerInteractionEvent p i) =
  let (eventName, body) = playerInteractionToDto p i
   in Just $ bridgeMessage "gameEvent" $ GameEventDto eventName body
mapEvent (PlayerAttackEvent p t) =
  Just $ bridgeMessage "gameEvent" $ GameEventDto "PlayerAttackEvent" (playerAttackToDto p t)
mapEvent (NpcAttackEvent npc t) =
  Just $ bridgeMessage "gameEvent" $ GameEventDto "NpcAttackEvent" (npcAttackDto npc t)
mapEvent (InternalNpcCannotReachTargetEvent npc t) =
  Nothing
mapEvent (NpcDeadEvent npc) =
  Just $ bridgeMessage "gameEvent" $ GameEventDto "NpcDeadEvent" (npcReferenceDto npc)
mapEvent (NpcEntityTickEvent npc) =
  Just $ bridgeMessage "gameEvent" $ GameEventDto "NpcEntityTickEvent" (npcReferenceDto npc)
mapEvent (PlayerCommandEvent playerIndex cmd args) =
  Just $ bridgeMessage "gameEvent" $ GameEventDto "PlayerCommandEvent" (commandDto playerIndex cmd args)
mapEvent (DropItemEvent playerId widgetId itemId index) =
  Just $ bridgeMessage "gameEvent" $ GameEventDto "DropItemEvent" (dropItemDto playerId widgetId itemId index)
mapEvent (ScriptInvokedEvent (ScriptInvocation functionName args)) =
  Just $ bridgeMessage "invokeScript" $ GameEventDto functionName (listValue id args)
mapEvent (InternalPlayerInteractionPendingPathingEvent _ _) =
  Nothing

data GameEventDto b = GameEventDto
  { event :: String,
    body :: b
  }
  deriving (Show, Generic)

instance (ToJSON b) => ToJSON (GameEventDto b)
