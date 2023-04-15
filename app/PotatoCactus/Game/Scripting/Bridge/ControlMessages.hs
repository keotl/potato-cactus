{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.ControlMessages where

import Data.Aeson (encode)
import Data.Aeson.Types (ToJSON)
import Data.ByteString.Lazy (ByteString, empty)
import GHC.Generics (Generic)
import PotatoCactus.Game.Scripting.Bridge.BridgeMessage (BridgeMessage, EmptyPayload (EmptyPayload), bridgeMessage)
import PotatoCactus.Game.Scripting.Bridge.Serialization.WorldDtoMapper (WorldDto, mapWorld)
import qualified PotatoCactus.Game.World as W

data BridgeInitOptions = BridgeInitOptions
  { workers :: Int
  }
  deriving (Show, Generic)

instance ToJSON BridgeInitOptions

initializeBridgeMessage :: BridgeInitOptions -> BridgeMessage BridgeInitOptions
initializeBridgeMessage = bridgeMessage "initialize"

doneSendingEventsMessage :: BridgeMessage EmptyPayload
doneSendingEventsMessage = bridgeMessage "doneSendingEvents" EmptyPayload

updateWorldContextMessage :: W.World -> BridgeMessage WorldDto
updateWorldContextMessage world =
  bridgeMessage "updateWorld" (mapWorld world)
