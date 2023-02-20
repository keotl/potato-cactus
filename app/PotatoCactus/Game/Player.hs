module PotatoCactus.Game.Player where

import Data.Foldable (fold)
import PotatoCactus.Game.Movement.MovementEntity (playerWalkMovement)
import qualified PotatoCactus.Game.Movement.MovementEntity as M (MovementEntity, issueWalkCommand)
import PotatoCactus.Game.Movement.PositionXY (PositionXY)
import PotatoCactus.Game.Movement.WalkingStep (WalkingStep)
import PotatoCactus.Game.PlayerUpdate.ChatMessage (ChatMessage)
import PotatoCactus.Game.PlayerUpdate.PlayerUpdate (PlayerUpdate)
import PotatoCactus.Game.PlayerUpdate.UpdateMask (PlayerUpdateMask, appearanceFlag)
import qualified PotatoCactus.Game.PlayerUpdate.UpdateMask as Mask
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position))

data Player = Player
  { serverIndex :: Int,
    username :: String,
    movement :: M.MovementEntity,
    updateMask :: PlayerUpdateMask,
    pendingUpdates :: [PlayerUpdate],
    chatMessage :: Maybe ChatMessage,
    skipUpdate_ :: Bool
  }
  deriving (Show)

instance GetPosition Player where
  getPosition = getPosition . movement

issueWalkCommand :: (PositionXY, Bool, [WalkingStep]) -> Player -> Player
issueWalkCommand (startPos, isRunning, steps) p =
  p {movement = M.issueWalkCommand (movement p) startPos steps}

create :: String -> Position -> Player
create username position =
  Player
    { serverIndex = -1,
      username = username,
      movement = playerWalkMovement position,
      updateMask = appearanceFlag,
      pendingUpdates = [],
      chatMessage = Nothing,
      skipUpdate_ = True
    }

queueUpdate :: Player -> PlayerUpdate -> Player
queueUpdate p update =
  p {pendingUpdates = update : pendingUpdates p}
