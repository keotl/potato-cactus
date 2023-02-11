module PotatoCactus.Boot.GameChannel where

import Control.Concurrent (Chan, newChan)
import Data.IORef (IORef)
import GHC.IO (unsafePerformIO)
import GHC.IORef (newIORef)
import PotatoCactus.Game.Player (Player)
import PotatoCactus.Game.World (ClientHandle (..))
import PotatoCactus.Game.Movement.WalkingStep (WalkingStep)
import PotatoCactus.Game.Movement.PositionXY (PositionXY)

data RegisterClientPayload = RegisterClientPayload
  { -- name :: "registerClient",
    clientHandle :: ClientHandle,
    player :: Player
  }

data GameChannelMessage
  = RegisterClientMessage RegisterClientPayload
  | UnregisterClientMessage String
  | PlayerWalkMessage String PositionXY Bool [WalkingStep]
  | InterfaceButtonClickMessage String Int
  | UpdateWorldMessage

-- gameChannelRef :: IORef (Chan GameChannelMessage)
-- gameChannelRef = unsafePerformIO $ newChan
-- # NOINLINE gameChannelRef #

gameChannel :: Chan GameChannelMessage
gameChannel = unsafePerformIO newChan
{-# NOINLINE gameChannel #-}
