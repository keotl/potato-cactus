{-# LANGUAGE LambdaCase #-}

module PotatoCactus.Game.World.CallbackScheduler (CallbackScheduler, queueCallback, callbacksForTick, clearCallbacksForTick, create) where

import Data.IntMap (IntMap, alter, delete, empty, (!?))
import Data.Maybe (fromMaybe)
import PotatoCactus.Game.Scripting.Actions.ScriptInvocation (ScriptInvocation)

data CallbackScheduler = CallbackScheduler
  { scheduled_ :: IntMap [ScriptInvocation]
  }
  deriving (Show)

create :: CallbackScheduler
create = CallbackScheduler empty

queueCallback :: CallbackScheduler -> ScriptInvocation -> Int -> CallbackScheduler
queueCallback scheduler script tick =
  scheduler
    { scheduled_ =
        alter
          ( \case
              Nothing -> Just [script]
              Just existing -> Just (script : existing)
          )
          tick
          (scheduled_ scheduler)
    }

callbacksForTick :: CallbackScheduler -> Int -> [ScriptInvocation]
callbacksForTick scheduler tick =
  fromMaybe [] (scheduled_ scheduler !? tick)

clearCallbacksForTick :: CallbackScheduler -> Int -> CallbackScheduler
clearCallbacksForTick scheduler tick =
  scheduler
    { scheduled_ = delete tick (scheduled_ scheduler)
    }
