module PotatoCactus.Game.Message.ObjectClickPayload where

import PotatoCactus.Game.Entity.Object.GameObject (GameObject)
import PotatoCactus.Game.Movement.PositionXY (PositionXY (PositionXY))

data ObjectClickPayload = ObjectClickPayload
  { object :: GameObject,
    actionIndex :: Int -- 1, 2, 3
  }
  deriving (Show)
