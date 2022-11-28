module PotatoCactus.Boot.GameChannel where

import Control.Concurrent (Chan, newChan)
import Data.IORef (IORef)
import GHC.IO (unsafePerformIO)
import GHC.IORef (newIORef)
import PotatoCactus.Game.World (ClientHandle (..))

newtype RegisterClientPayload = RegisterClientPayload
  { -- name :: "registerClient",
    clientHandle :: ClientHandle
  }

data GameChannelMessage
  = RegisterClientMessage RegisterClientPayload
  | UpdateWorldMessage

-- gameChannelRef :: IORef (Chan GameChannelMessage)
-- gameChannelRef = unsafePerformIO $ newChan
-- # NOINLINE gameChannelRef #

gameChannel :: Chan GameChannelMessage
gameChannel = unsafePerformIO newChan
{-# NOINLINE gameChannel #-}
