module PotatoCactus.Game.Entity.Object.DynamicObject where

import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))

-- Represents an overlay over the static object set at any given time.
-- Static Set + Dynamic Set = Objects as they are visible to clients
data DynamicObject
  = Added GameObject -- new object
  | Replacing GameObject GameObject -- new object, old object
  | Removed GameObject -- old object
  deriving (Show, Eq)
