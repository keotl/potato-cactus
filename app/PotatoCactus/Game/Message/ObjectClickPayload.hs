module PotatoCactus.Game.Message.ObjectClickPayload where

import PotatoCactus.Game.Movement.PositionXY (PositionXY (PositionXY))

data ObjectClickPayload = ObjectClickPayload
  { objectId :: Int,
    position :: PositionXY,
    index :: Int -- 1, 2, 3
  }
  deriving (Show)
