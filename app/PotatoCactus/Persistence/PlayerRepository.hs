module PotatoCactus.Persistence.PlayerRepository where

import PotatoCactus.Game.Definitions.EquipmentDefinitions (chestSlot, legsSlot)
import PotatoCactus.Game.ItemContainer (ItemContainer (ItemContainer), ItemStack (ItemStack), addItems, playerInventory)
import PotatoCactus.Game.Movement.MovementEntity (playerWalkMovement)
import PotatoCactus.Game.Player (Player (Player, inventory, movement, username))
import qualified PotatoCactus.Game.Player as Player
import PotatoCactus.Game.Position (Position (Position))

retrievePlayer :: String -> IO (Maybe Player)
retrievePlayer username =
  return $ Just (mockPlayer_ username)

mockPlayer_ :: String -> Player
mockPlayer_ username = (Player.create username mockPosition_) {inventory = mockInventory_}

mockPosition_ :: Position
mockPosition_ = Position 3167 3304 0 -- windmill testing
-- mockPosition_ = Position 3165 3307 2 -- windmill top
-- mockPosition_ = Position 3222 3218 0 -- lumbridge castle courtyard

mockInventory_ :: ItemContainer
mockInventory_ =
  addItems
    playerInventory
    [ ItemStack 1115 1,
      ItemStack 1067 1,
      ItemStack 1137 1,
      ItemStack 1155 1,
      ItemStack 617 100000,
      ItemStack 1947 1
    ]
