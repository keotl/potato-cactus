module PotatoCactus.Game.Player where

import Data.Foldable (fold)
import PotatoCactus.Game.ItemContainer (ItemContainer, playerEquipmentContainer, playerInventory)
import PotatoCactus.Game.Movement.MovementEntity (playerWalkMovement)
import qualified PotatoCactus.Game.Movement.MovementEntity as M (MovementEntity, issueWalkCommand)
import PotatoCactus.Game.Movement.PositionXY (PositionXY)
import PotatoCactus.Game.Movement.WalkingStep (WalkingStep)
import PotatoCactus.Game.PlayerUpdate.Appearance (PlayerAppearance, defaultPlayerAppearance)
import PotatoCactus.Game.PlayerUpdate.ChatMessage (ChatMessage)
import PotatoCactus.Game.PlayerUpdate.Equipment (Equipment (Equipment))
import PotatoCactus.Game.PlayerUpdate.PlayerUpdate (PlayerUpdate)
import PotatoCactus.Game.PlayerUpdate.UpdateMask (PlayerUpdateMask, appearanceFlag)
import qualified PotatoCactus.Game.PlayerUpdate.UpdateMask as Mask
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position))

data Player = Player
  { serverIndex :: Int,
    username :: String,
    appearance :: PlayerAppearance,
    movement :: M.MovementEntity,
    updateMask :: PlayerUpdateMask,
    pendingUpdates :: [PlayerUpdate],
    chatMessage :: Maybe ChatMessage,
    inventory :: ItemContainer,
    equipment :: Equipment,
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
      appearance = defaultPlayerAppearance,
      movement = playerWalkMovement position,
      updateMask = appearanceFlag,
      pendingUpdates = [],
      chatMessage = Nothing,
      inventory = playerInventory,
      equipment = Equipment playerEquipmentContainer,
      skipUpdate_ = True
    }

queueUpdate :: Player -> PlayerUpdate -> Player
queueUpdate p update =
  p {pendingUpdates = update : pendingUpdates p}
