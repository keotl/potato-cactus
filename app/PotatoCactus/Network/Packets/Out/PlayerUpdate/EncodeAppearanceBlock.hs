module PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeAppearanceBlock where

import Data.Binary (Word16)
import Data.Binary.BitPut (putNBits, runBitPut)
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Game.Player (Player (appearance, equipment, username))
import PotatoCactus.Game.PlayerUpdate.Appearance (Gender (Male), PlayerAppearance (playerArms, playerBeard, playerChest, playerFeet, playerFeetColour, playerGender, playerHairColour, playerHands, playerHead, playerLegColour, playerLegs, playerSkinColour, playerTorsoColour))
import PotatoCactus.Game.PlayerUpdate.Equipment (EquipmentSlot, amuletSlot, capeSlot, chestSlot, feetSlot, handsSlot, headSlot, itemIdAtSlot, legsSlot, shieldSlot, shouldShowModel, weaponSlot)
import PotatoCactus.Game.World (World)
import PotatoCactus.Network.Binary (encodeToBase37, toShort_, toWord_)

encodeAppearanceBlock :: Player -> World -> ByteString
encodeAppearanceBlock player world =
  let body =
        toStrict $
          runBitPut
            ( do
                putNBits 8 $ toWord_ 0 -- gender
                -- putNBits 8 $ toWord_ (-1) -- prayer
                putNBits 8 $ toWord_ 0 -- head icon
                -- no player transformId

                -- equipment
                putNBits 16 $ toShort_ (512 + itemIdAtSlot headSlot (equipment player))
                putNBits 16 $ toShort_ (512 + itemIdAtSlot capeSlot (equipment player))
                putNBits 16 $ toShort_ (512 + itemIdAtSlot amuletSlot (equipment player))
                putNBits 16 $ toShort_ (512 + itemIdAtSlot weaponSlot (equipment player))

                -- chest model
                putNBits 16 $ mapEquipment_ chestSlot playerChest player
                putNBits 16 $ toShort_ (512 + itemIdAtSlot shieldSlot (equipment player))
                if shouldShowModel chestSlot (equipment player)
                  then putNBits 16 $ toShort_ (256 + (playerArms . appearance $ player))
                  else putNBits 8 $ toWord_ 0

                putNBits 16 $ mapEquipment_ legsSlot playerLegs player

                if shouldShowModel headSlot (equipment player)
                  then putNBits 16 $ toShort_ (256 + (playerHead . appearance $ player))
                  else putNBits 8 $ toWord_ 0

                putNBits 16 $ mapEquipment_ handsSlot playerHands player
                putNBits 16 $ mapEquipment_ feetSlot playerFeet player

                case (playerGender (appearance player), shouldShowModel headSlot (equipment player)) of
                  (Male, True) -> putNBits 16 $ toShort_ (256 + playerBeard (appearance player))
                  (_, _) -> putNBits 8 $ toWord_ 0

                -- model colours
                putNBits 8 $ playerHairColour (appearance player)
                putNBits 8 $ playerTorsoColour (appearance player)
                putNBits 8 $ playerLegColour (appearance player)
                putNBits 8 $ playerFeetColour (appearance player)
                putNBits 8 $ playerSkinColour (appearance player)

                -- model animations
                putNBits 16 $ toShort_ 808 -- standing
                putNBits 16 $ toShort_ 823 -- standing turn
                putNBits 16 $ toShort_ 819 -- walking
                putNBits 16 $ toShort_ 820 -- turn 180
                putNBits 16 $ toShort_ 821 -- turn 90 CW
                putNBits 16 $ toShort_ 822 -- turn 90 CCW
                putNBits 16 $ toShort_ 824 -- running
                putNBits 64 $ encodeToBase37 $ username player -- username hash
                putNBits 8 $ toWord_ 3 -- combat level
                putNBits 16 $ toShort_ 0 -- skill level for games room
            )
   in ByteString.concat [pack [fromIntegral (- ByteString.length body)], body]

mapEquipment_ :: EquipmentSlot -> (PlayerAppearance -> Int) -> Player -> Word16
mapEquipment_ equipmentSlot bodyPart player =
  case itemIdAtSlot equipmentSlot (equipment player) of
    0 -> 256 + (fromIntegral . bodyPart . appearance $ player)
    x -> 512 + fromIntegral x
