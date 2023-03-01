module PotatoCactus.Boot.GameChannel where

import Control.Concurrent (Chan, newChan)
import Data.IORef (IORef)
import GHC.IO (unsafePerformIO)
import GHC.IORef (newIORef)
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage)

gameChannel :: Chan GameChannelMessage
gameChannel = unsafePerformIO newChan
{-# NOINLINE gameChannel #-}
