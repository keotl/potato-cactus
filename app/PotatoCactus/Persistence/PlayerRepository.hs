module PotatoCactus.Persistence.PlayerRepository where

import PotatoCactus.Game.Item (Item (Item), armourItem, stackableItem)
import PotatoCactus.Game.ItemContainer (ItemContainer (ItemContainer), ItemStack (ItemStack), addItems, playerInventory)
import PotatoCactus.Game.Movement.MovementEntity (playerWalkMovement)
import PotatoCactus.Game.Player (Player (Player, inventory, movement, username))
import qualified PotatoCactus.Game.Player as Player
import PotatoCactus.Game.PlayerUpdate.Equipment (chestSlot, headSlot, legsSlot)
import PotatoCactus.Game.Position (Position (Position))

retrievePlayer :: String -> IO (Maybe Player)
retrievePlayer username = do
  return $ Just (mockPlayer_ username)

mockPlayer_ :: String -> Player
mockPlayer_ username = (Player.create username mockPosition_) {inventory = mockInventory_}

mockPosition_ :: Position
mockPosition_ = Position 3093 3244 0

mockInventory_ :: ItemContainer
mockInventory_ =
  addItems
    playerInventory
    [ ItemStack (armourItem 1115 chestSlot True) 1,
      ItemStack (armourItem 1067 legsSlot True) 1,
      ItemStack (armourItem 1137 headSlot False) 1,
      ItemStack (armourItem 1155 headSlot True) 1,
      ItemStack (stackableItem 617) 100000
    ]
