module PotatoCactus.Game.Player where

import Data.Binary (Word32, Word8)
import Data.Bits ((.&.), (.|.))
import Data.Foldable (fold)
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity, CombatTarget (NpcTarget))
import qualified PotatoCactus.Game.Combat.CombatEntity as CombatEntity
import PotatoCactus.Game.Combat.Hit (Hit)
import PotatoCactus.Game.Definitions.Types.ItemDefinition (ItemId)
import qualified PotatoCactus.Game.Entity.Animation.Animation as Anim
import qualified PotatoCactus.Game.Entity.EntityData as EntityData
import PotatoCactus.Game.Entity.Interaction.Interaction (Interaction)
import qualified PotatoCactus.Game.Entity.Interaction.Interaction as Interaction
import PotatoCactus.Game.Entity.Npc.Npc (NpcIndex)
import PotatoCactus.Game.Interface.InterfaceController (InterfaceController, clearStandardInterfaces, configureInterface)
import qualified PotatoCactus.Game.Interface.InterfaceController as IC
import PotatoCactus.Game.ItemContainer (ItemContainer, playerEquipmentContainer, playerInventory)
import qualified PotatoCactus.Game.ItemContainer as Container
import qualified PotatoCactus.Game.Movement.PlayerMovement as M
import PotatoCactus.Game.Movement.PositionXY (PositionXY)
import PotatoCactus.Game.Movement.WalkingStep (WalkingStep)
import PotatoCactus.Game.PlayerUpdate.Appearance (PlayerAppearance, defaultPlayerAppearance)
import PotatoCactus.Game.PlayerUpdate.ChatMessage (ChatMessage)
import PotatoCactus.Game.PlayerUpdate.Equipment (Equipment (Equipment))
import PotatoCactus.Game.PlayerUpdate.PlayerUpdate (PlayerUpdate)
import PotatoCactus.Game.PlayerUpdate.UpdateMask (PlayerUpdateMask, animationFlag, appearanceFlag, primaryHealthUpdateFlag, secondaryHealthUpdateFlag)
import qualified PotatoCactus.Game.PlayerUpdate.UpdateMask as Mask
import qualified PotatoCactus.Game.PlayerUpdate.VarpSet as VarpSet
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position))
import PotatoCactus.Game.Scripting.Actions.CreateInterface (CreateInterfaceRequest, InterfaceType)
import PotatoCactus.Game.Typing (Keyable (key))

type PlayerIndex = Int

data Player = Player
  { serverIndex :: PlayerIndex,
    username :: String,
    appearance :: PlayerAppearance,
    movement :: M.PlayerMovement,
    updateMask :: PlayerUpdateMask,
    pendingUpdates :: [PlayerUpdate],
    chatMessage :: Maybe ChatMessage, -- Overhead text
    inventory :: ItemContainer,
    equipment :: Equipment,
    interaction :: Interaction,
    combat :: CombatEntity,
    animation :: Maybe Anim.Animation,
    chatboxMessages :: [String],
    interfaces :: InterfaceController,
    entityData :: EntityData.EntityData,
    varps :: VarpSet.VarpSet,
    droppedItemIndices :: [Int],
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
    { movement = M.queueWalk (movement p) startPos steps,
      combat = CombatEntity.clearTarget . combat $ p,
      interaction = Interaction.create,
      interfaces = clearStandardInterfaces . interfaces $ p
    }

create :: String -> Position -> Player
create username position =
  Player
    { serverIndex = -1,
      username = username,
      appearance = defaultPlayerAppearance,
      movement = M.create position,
      updateMask = appearanceFlag,
      pendingUpdates = [],
      chatMessage = Nothing,
      inventory = playerInventory,
      equipment = Equipment playerEquipmentContainer,
      interaction = Interaction.create,
      combat = CombatEntity.create 10,
      animation = Nothing,
      chatboxMessages = [],
      interfaces = IC.create,
      entityData = EntityData.create,
      varps = VarpSet.create,
      droppedItemIndices = [],
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
  p {combat = CombatEntity.setAttackCooldown (combat p) 3}

setAttackTarget :: CombatEntity.CombatTarget -> Player -> Player
setAttackTarget target p =
  p {combat = CombatEntity.setTarget (combat p) target}

setAnimation :: Anim.Animation -> Player -> Player
setAnimation anim player =
  player
    { animation = Anim.setAnimation (animation player) anim,
      updateMask = updateMask player .|. animationFlag
    }

sendChatboxMessage :: Player -> String -> Player
sendChatboxMessage p msg =
  p {chatboxMessages = chatboxMessages p ++ [msg]}

setPosition :: Player -> Position -> Player
setPosition p pos =
  p
    { movement = M.setPosition (movement p) pos
    }

createInterface :: Player -> CreateInterfaceRequest -> Player
createInterface p req =
  p
    { interfaces = configureInterface (interfaces p) req
    }

clearStandardInterface :: Player -> Player
clearStandardInterface p =
  p
    { interfaces = clearStandardInterfaces $ interfaces p
    }

updateEntityData :: Player -> (EntityData.EntityData -> EntityData.EntityData) -> Player
updateEntityData p transform =
  p
    { entityData = transform (entityData p)
    }

giveItem :: Player -> Container.ItemStack -> Player
giveItem p stack =
  p
    { inventory = Container.addItem (inventory p) stack
    }

subtractItem :: Player -> (ItemId, Int) -> Player
subtractItem p stack =
  p
    { inventory = Container.subtractItem (inventory p) stack
    }

removeItemStack :: Player -> (ItemId, Int) -> Player
removeItemStack p stack =
  p
    { inventory = Container.removeStack (inventory p) stack
    }

setVarp :: Player -> (VarpSet.VarpId, Word32) -> Player
setVarp p operation =
  p
    { varps = VarpSet.setVarp operation (varps p)
    }

setVarbit :: Player -> (VarpSet.VarpId, Word8, Word8, Word32) -> Player
setVarbit p operation =
  p
    { varps = VarpSet.setVarbit operation (varps p)
    }
