module PotatoCactus.Game.Typing where

-- On new game tick
class Advance t where
  advance :: t -> t

class ToNumeric t where
  toNumeric :: t -> Int

class Keyable t where
  key :: t -> String
