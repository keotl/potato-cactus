module PotatoCactus.Game.Entity.Interaction.State where

data InteractionState
  = Pending -- Moving towards target
  | InProgress -- Ongoing, for long-running interactions
  deriving (Show)
