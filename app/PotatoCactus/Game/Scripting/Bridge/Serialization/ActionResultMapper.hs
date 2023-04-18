{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.ActionResultMapper (mapResult) where

import Data.Aeson (FromJSON, Result (Error, Success), Value (Object), decode, decodeStrict, (.:))
import Data.Aeson.Types (Object, parse)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import GHC.Generics (Generic)
import PotatoCactus.Game.Scripting.ScriptUpdates (ScriptActionResult (DummyEvent, InternalNoop, InternalProcessingComplete))

mapResult :: ByteString -> ScriptActionResult
mapResult bytes =
  case decodeStrict bytes :: Maybe DecodedMessage of
    Just val -> decodeBody (op val) (body val)
    Nothing -> InternalNoop

decodeBody :: String -> Object -> ScriptActionResult
decodeBody "internal_processingComplete" _ =
  InternalProcessingComplete
decodeBody "dummyEvent" body =
  case parse
    ( \obj -> do
        key <- obj .: "key"
        return (DummyEvent key)
    )
    body of
    Error msg -> InternalNoop
    Success decoded -> decoded
decodeBody _ _ = InternalNoop

data DecodedMessage = DecodedMessage
  { op :: String,
    body :: Object
  }
  deriving (Generic, FromJSON)
