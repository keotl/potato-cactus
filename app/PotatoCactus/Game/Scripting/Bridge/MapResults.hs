module PotatoCactus.Game.Scripting.Bridge.MapResults (mapResult) where

import PotatoCactus.Game.Scripting.ScriptUpdates (ScriptActionResult (InternalNoop, InternalProcessingComplete))

mapResult :: String -> ScriptActionResult
mapResult "DONE" = InternalProcessingComplete
mapResult _ = InternalNoop
