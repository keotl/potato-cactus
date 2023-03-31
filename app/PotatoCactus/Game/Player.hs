module PotatoCactus.Game.Player where

import Data.Bits ((.&.), (.|.))
import Data.Foldable (fold)
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity)
import qualified PotatoCactus.Game.Combat.CombatEntity as CombatEntity
import PotatoCactus.Game.Combat.Hit (Hit)
import qualified PotatoCactus.Game.Entity.Animation.Animation as Anim
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction)
import qualified PotatoCactus.Game.Entity.Interaction.Interaction as Interaction
import PotatoCactus.Game.ItemContainer (ItemContainer, playerEquipmentContainer, playerInventory)
import PotatoCactus.Game.Movement.MovementEntity (playerWalkMovement)
import qualified PotatoCactus.Game.Movement.MovementEntity as M (MovementEntity, issueWalkCommand)
import PotatoCactus.Game.Movement.PositionXY (PositionXY)
import PotatoCactus.Game.Movement.WalkingStep (WalkingStep)
import PotatoCactus.Game.PlayerUpdate.Appearance (PlayerAppearance, defaultPlayerAppearance)
import PotatoCactus.Game.PlayerUpdate.ChatMessage (ChatMessage)
import PotatoCactus.Game.PlayerUpdate.Equipment (Equipment (Equipment))
import PotatoCactus.Game.PlayerUpdate.PlayerUpdate (PlayerUpdate)
import PotatoCactus.Game.PlayerUpdate.UpdateMask (PlayerUpdateMask, animationFlag, appearanceFlag, primaryHealthUpdateFlag, secondaryHealthUpdateFlag)
import qualified PotatoCactus.Game.PlayerUpdate.UpdateMask as Mask
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position))
import PotatoCactus.Game.Typing (Keyable (key))

type PlayerIndex = Int

data Player = Player
  { serverIndex :: PlayerIndex,
    username :: String,
    appearance :: PlayerAppearance,
    movement :: M.MovementEntity,
    updateMask :: PlayerUpdateMask,
    pendingUpdates :: [PlayerUpdate],
    chatMessage :: Maybe ChatMessage,
    inventory :: ItemContainer,
    equipment :: Equipment,
    interaction :: Interaction,
    combat :: CombatEntity,
    animation :: Maybe Anim.Animation,
    skipUpdate_ :: Bool
  }
  deriving (Show)

instance GetPosition Player where
  getPosition = getPosition . movement

instance Keyable Player where
  key = username

issueWalkCommand :: (PositionXY, Bool, [WalkingStep]) -> Player -> Player
issueWalkCommand (startPos, isRunning, steps) p =
  p
    { movement = M.issueWalkCommand (movement p) startPos steps,
      combat = CombatEntity.clearTarget . combat $ p
    }

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
      interaction = Interaction.create,
      combat = CombatEntity.create 10,
      animation = Nothing,
      skipUpdate_ = True
    }

queueUpdate :: Player -> PlayerUpdate -> Player
queueUpdate p update =
  p
    { pendingUpdates = update : pendingUpdates p,
      interaction = Interaction.create,
      combat = CombatEntity.clearTarget (combat p)
    }

applyHit :: CombatEntity.CombatTarget -> Hit -> Player -> Player
applyHit sourceEntity hit player =
  player
    { combat = CombatEntity.applyHit (combat player) sourceEntity hit,
      updateMask =
        if (updateMask player .&. primaryHealthUpdateFlag) > 0
          then updateMask player .|. secondaryHealthUpdateFlag
          else updateMask player .|. primaryHealthUpdateFlag
    }

-- Sets the attack cooldown based on equipped items
setAttackCooldown :: Player -> Player
setAttackCooldown p =
  p {combat = CombatEntity.setAttackCooldown (combat p) 10}

setAttackTarget :: Player -> CombatEntity.CombatTarget -> Player
setAttackTarget p target =
  p {combat = CombatEntity.setTarget (combat p) target}

setAnimation :: Anim.Animation -> Player -> Player
setAnimation anim player =
  player
    { animation = Anim.setAnimation (animation player) anim,
      updateMask = updateMask player .|. animationFlag
    }
