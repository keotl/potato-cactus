{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Bridge.BridgeMessage (bridgeMessage, BridgeMessage, EmptyPayload (EmptyPayload)) where

import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (emptyObject, toJSON)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.ByteString.Lazy.UTF8 (fromString)
import GHC.Generics (Generic)

data BridgeMessage b = BridgeMessage
  { op :: String,
    body :: b
  }
  deriving (Show, Generic)

instance (ToJSON b) => ToJSON (BridgeMessage b)

data EmptyPayload = EmptyPayload deriving (Show, Generic)

instance ToJSON EmptyPayload where
  toJSON _ = emptyObject

bridgeMessage :: ToJSON b => String -> b -> BridgeMessage b
bridgeMessage = BridgeMessage
