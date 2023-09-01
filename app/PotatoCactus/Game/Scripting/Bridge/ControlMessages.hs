{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.ControlMessages where

import Data.Aeson (encode)
import Data.Aeson.Types (ToJSON)
import Data.ByteString.Lazy (ByteString, empty)
import GHC.Generics (Generic)
import PotatoCactus.Game.Definitions.StaticGameObjectSet (StaticGameObjectSet)
import PotatoCactus.Game.Scripting.Bridge.BridgeMessage (BridgeMessage, EmptyPayload (EmptyPayload), bridgeMessage)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.StaticGameObjectSetDto (StaticGameObjectSetDto, staticGameObjectSetDto)
import PotatoCactus.Game.Scripting.Bridge.Serialization.Models.WorldDto (WorldDto, worldToDto)
import qualified PotatoCactus.Game.World as W

data BridgeInitOptions = BridgeInitOptions
  { workers :: Int,
    scriptPaths :: [String]
  }
  deriving (Show, Generic)

instance ToJSON BridgeInitOptions

initializeBridgeMessage :: BridgeInitOptions -> BridgeMessage BridgeInitOptions
initializeBridgeMessage = bridgeMessage "initialize"

doneSendingEventsMessage :: BridgeMessage EmptyPayload
doneSendingEventsMessage = bridgeMessage "doneSendingEvents" EmptyPayload

updateWorldContextMessage :: W.World -> BridgeMessage WorldDto
updateWorldContextMessage world =
  bridgeMessage "updateWorld" (worldToDto world)

setStaticObjectSetMessage :: StaticGameObjectSet -> BridgeMessage StaticGameObjectSetDto
setStaticObjectSetMessage objectSet =
  bridgeMessage "setStaticObjectSet" (staticGameObjectSetDto objectSet)
