module PotatoCactus.Persistence.PlayerRepository where

import PotatoCactus.Game.Movement.MovementEntity (playerWalkMovement)
import PotatoCactus.Game.Player (Player (Player, movement, username))
import qualified PotatoCactus.Game.Player as Player
import PotatoCactus.Game.Position (Position (Position))

retrievePlayer :: String -> IO (Maybe Player)
retrievePlayer username = do
  return $ Just (mockPlayer_ username)

mockPlayer_ :: String -> Player
mockPlayer_ username = Player.create username mockPosition_

mockPosition_ :: Position
-- mockPosition_ = Position 3198 3253 0
mockPosition_ = Position 3093 3244 0
