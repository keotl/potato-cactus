module PotatoCactus.Game.Typing where

-- On new game tick
class Advance t where
  advance :: t -> t

class ToNumeric t where
  toNumeric :: t -> Int

class Keyable t where
  key :: t -> String

-- Whether entity is active in the world and shown to players (i.e. not dead)
class IsEntityActive t where
  isEntityActive :: t -> Bool

-- Should remove entity from moblist
class ShouldDiscard t where
  shouldDiscard :: t -> Bool
