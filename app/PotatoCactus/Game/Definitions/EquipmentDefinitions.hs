module PotatoCactus.Game.Definitions.EquipmentDefinitions where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.IntMap (IntMap, empty, insert, size, (!?))
import GHC.IO (unsafePerformIO)
import GHC.IORef (IORef)
import PotatoCactus.Game.Definitions.ItemDefinitions (ItemId)

type EquipmentSlot = Int
data EquipmentDefinition = EquipmentDefinition
  { itemId :: ItemId,
    slot :: EquipmentSlot,
    shouldHideModelWhenEquipped :: Bool
  }

headSlot :: EquipmentSlot
headSlot = 0

capeSlot :: EquipmentSlot
capeSlot = 1

amuletSlot :: EquipmentSlot
amuletSlot = 2

weaponSlot :: EquipmentSlot
weaponSlot = 3

chestSlot :: EquipmentSlot
chestSlot = 4

shieldSlot :: EquipmentSlot
shieldSlot = 5

legsSlot :: EquipmentSlot
legsSlot = 7

handsSlot :: EquipmentSlot
handsSlot = 9

feetSlot :: EquipmentSlot
feetSlot = 10

ringSlot :: EquipmentSlot
ringSlot = 12

ammoSlot :: EquipmentSlot
ammoSlot = 13

itemDb :: IORef (IntMap EquipmentDefinition)
{-# NOINLINE itemDb #-}
itemDb = unsafePerformIO $ newIORef empty

equipmentDefinition :: ItemId -> Maybe EquipmentDefinition
equipmentDefinition itemId =
  unsafePerformIO
    ( do
        db <- readIORef itemDb
        return (db !? itemId)
    )

initializeEquipmentDefs :: IO Int
initializeEquipmentDefs = do
  db <- readIORef itemDb
  -- TODO - read from file  - keotl 2023-03-01
  let updated =
        foldl
          (\a e -> e a)
          db
          [ addMockItem_ 1115 chestSlot True,
            addMockItem_ 1067 legsSlot True,
            addMockItem_ 1137 headSlot False,
            addMockItem_ 1155 headSlot True
          ]
  writeIORef itemDb updated

  return (size updated)

addMockItem_ :: Int -> EquipmentSlot -> Bool -> (IntMap EquipmentDefinition -> IntMap EquipmentDefinition)
addMockItem_ itemId slot hidesModel =
  insert itemId (EquipmentDefinition itemId slot hidesModel)
