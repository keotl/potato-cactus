module PotatoCactus.Game.Scripting.Actions.ScriptInvocation where

import Data.Aeson (Value)

data ScriptInvocation = ScriptInvocation
  { f :: String,
    args :: [Value]
  }
  deriving (Show)
