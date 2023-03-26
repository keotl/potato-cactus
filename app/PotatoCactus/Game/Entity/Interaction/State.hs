module PotatoCactus.Game.Entity.Interaction.State where

data InteractionState
  = Pending -- Moving towards target
  | PendingPathing -- Got to initial walk point, but target has moved
  | InProgress -- Ongoing, for long-running interactions
  deriving (Show)
