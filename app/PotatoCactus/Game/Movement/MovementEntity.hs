module PotatoCactus.Game.Movement.MovementEntity where

import PotatoCactus.Game.Movement.PlayerWalkMovement
import qualified PotatoCactus.Game.Movement.PlayerWalkMovement as PlayerWalkMovement
import PotatoCactus.Game.Movement.StaticMovement (StaticMovement)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position)
import PotatoCactus.Game.Typing (Advance (advance))

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
playerWalkMovement = PlayerWalkMovement_ . PlayerWalkMovement.create
