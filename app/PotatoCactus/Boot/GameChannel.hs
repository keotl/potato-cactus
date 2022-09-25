module PotatoCactus.Boot.GameChannel where

import Control.Concurrent (Chan, newChan)
import Data.IORef (IORef)
import GHC.IO (unsafePerformIO)
import GHC.IORef (newIORef)
import PotatoCactus.Network.ClientHandle (ClientHandle)

newtype RegisterClientMessage = RegisterClientMessage
  { clientHandle :: ClientHandle
  }

type GameChannelMessage = RegisterClientMessage -- TODO create types

-- gameChannelRef :: IORef (Chan GameChannelMessage)
-- gameChannelRef = unsafePerformIO $ newChan
-- # NOINLINE gameChannelRef #

gameChannel :: Chan GameChannelMessage
gameChannel = unsafePerformIO newChan
{-# NOINLINE gameChannel #-}
