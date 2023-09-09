module PotatoCactus.Game.Entity.Object.DynamicObject where

import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))

-- Represents an overlay over the static object set at any given time.
-- Static Set + Dynamic Set = Objects as they are visible to clients
data DynamicObject
  = Added GameObject -- new object
  | Replacing GameObject GameObject -- new object, old object
  | Removed GameObject -- old object
  deriving (Show, Eq)

data VisibleObject
  = Visible GameObject -- Object is interactable by a user
  | Hidden -- Object has been marked as removed, and so no interaction is possible.
  | None -- No matching object was found in the dynamic set, continue lookup in static set.
  deriving (Show, Eq)

hasFoundMatch :: VisibleObject -> Bool
hasFoundMatch (Visible _) = True
hasFoundMatch Hidden = True
hasFoundMatch None = False
