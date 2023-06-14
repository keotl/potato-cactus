{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Scripting.Actions.ScriptInvocation where

import Data.Aeson (FromJSON, Value)
import GHC.Generics (Generic)

data ScriptInvocation = ScriptInvocation
  { f :: String,
    args :: [Value]
  }
  deriving (Show, Generic)

instance FromJSON ScriptInvocation
