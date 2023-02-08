module PotatoCactus.Game.Typing where

-- On new game tick
class Advance t where
  advance :: t -> t
