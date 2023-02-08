module PotatoCactus.Game.Movement.MovementEntity (MovementEntity, playerWalkMovement) where

import PotatoCactus.Game.Movement.PlayerWalkMovement
import PotatoCactus.Game.Movement.StaticMovement (StaticMovement)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position)
import PotatoCactus.Game.Typing (Advance (advance))

-- data MovementEntity where
data MovementEntity = PlayerWalkMovement_ PlayerWalkMovement | StaticMovement_ StaticMovement deriving (Show)

instance GetPosition MovementEntity where
  getPosition t = case t of
    PlayerWalkMovement_ m -> getPosition m
    StaticMovement_ m -> getPosition m

instance Advance MovementEntity where
  advance t = case t of
    PlayerWalkMovement_ m -> PlayerWalkMovement_ $ advance m
    StaticMovement_ m -> StaticMovement_ $ advance m

playerWalkMovement :: Position -> MovementEntity
playerWalkMovement pos =
  PlayerWalkMovement_
    PlayerWalkMovement
      { position_ = pos,
        queue_ = []
      }
