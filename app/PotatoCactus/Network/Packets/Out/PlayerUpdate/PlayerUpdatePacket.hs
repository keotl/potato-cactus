{-# LANGUAGE BlockArguments #-}

module PotatoCactus.Network.Packets.Out.PlayerUpdate.PlayerUpdatePacket where

import Data.Binary (Word16, Word8)
import Data.Binary.BitPut (BitPut, putBit, putBits, putByteString, putNBits, runBitPut)
import Data.Bits (Bits ((.|.)), (.&.))
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as Data.Binary
import Data.List (find)
import GHC.RTS.Flags (ProfFlags (modSelector))
import PotatoCactus.Client.LocalPlayerList (LocalPlayer (LocalPlayer), LocalPlayerList, LocalPlayerStatus (Added, Removed, Retained))
import PotatoCactus.Game.Player as P (Player (serverIndex, updateMask, username))
import PotatoCactus.Game.PlayerUpdate.UpdateMask (PlayerUpdateMask, appearanceFlag, chatFlag)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (x, y))
import PotatoCactus.Game.World as W
  ( ClientHandle (username),
    World (players),
  )
import PotatoCactus.Network.Binary (encodeToBase37, toByte, toShortLE_, toShort_, toWord_)
import PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeBlock (addBlockIfRequired)
import PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeChatUpdate (encodeChatUpdateBlock)
import PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodePlayerMovement (encodePlayerMovement)
import PotatoCactus.Network.Packets.Packet (varShortPacket)

playerUpdatePacket :: Player -> LocalPlayerList -> World -> ByteString
playerUpdatePacket player localPlayers world =
  varShortPacket
    81
    do
      let myBlockMsg = blockMsg_ player world
       in let blockMsg = ByteString.concat (myBlockMsg : otherPlayerBlocks_ localPlayers world)
           in do
                putByteString $
                  beforeBlockMsg_
                    player
                    localPlayers
                    world
                    (ByteString.length blockMsg > 0)

                putByteString blockMsg

beforeBlockMsg_ :: Player -> LocalPlayerList -> World -> Bool -> ByteString
beforeBlockMsg_ player localPlayers world hasUpdateBlocks =
  toStrict $
    runBitPut
      ( do
          encodePlayerMovement player
          putNBits 8 $ toWord_ (clientKnownPlayers_ localPlayers) -- localplayers list length
          otherExistingPlayerMovement_ localPlayers
          addOtherNewPlayers_ player localPlayers
          if hasUpdateBlocks then putNBits 11 $ toShort_ 2047 else putNBits 0 $ toWord_ 0
      )

clientKnownPlayers_ :: LocalPlayerList -> Int
clientKnownPlayers_ localPlayers =
  length
    ( filter
        ( \(LocalPlayer p status) -> case status of
            Added -> False
            _ -> True
        )
        localPlayers
    )

otherExistingPlayerMovement_ :: LocalPlayerList -> BitPut
otherExistingPlayerMovement_ = mapM_ mapOtherPlayerMovementUpdate_

mapOtherPlayerMovementUpdate_ :: LocalPlayer -> BitPut
mapOtherPlayerMovementUpdate_ (LocalPlayer _ Added) = putNBits 0 $ toWord_ 0
mapOtherPlayerMovementUpdate_ (LocalPlayer _ Removed) = do
  putBit True
  putNBits 2 $ toWord_ 3
mapOtherPlayerMovementUpdate_ (LocalPlayer p Retained) = encodePlayerMovement p

addOtherNewPlayers_ :: Player -> LocalPlayerList -> BitPut
addOtherNewPlayers_ refPlayer = mapM_ (mapAddOtherNewPlayer_ refPlayer)

mapAddOtherNewPlayer_ :: Player -> LocalPlayer -> BitPut
mapAddOtherNewPlayer_ refPlayer (LocalPlayer p Added) = do
  putNBits 11 $ toShort_ (serverIndex p)
  putBit True
  putBit True
  let deltaY = (y . getPosition) p - (y . getPosition) refPlayer
   in putNBits 5 $ toWord_ deltaY
  let deltaX = (x . getPosition) p - (x . getPosition) refPlayer
   in putNBits 5 $ toWord_ deltaX
mapAddOtherNewPlayer_ _ (LocalPlayer _ _) = putNBits 0 $ toWord_ 0

otherPlayerBlocks_ :: LocalPlayerList -> World -> [ByteString]
otherPlayerBlocks_ otherPlayers world =
  map (`otherPlayerBlock_` world) otherPlayers

otherPlayerBlock_ :: LocalPlayer -> World -> ByteString
otherPlayerBlock_ (LocalPlayer _ Removed) _ = empty
otherPlayerBlock_ (LocalPlayer p Added) world =
  blockMsg_ (p {updateMask = (updateMask p) .|. appearanceFlag}) world
otherPlayerBlock_ (LocalPlayer p Retained) world =
  blockMsg_ p world

blockMsg_ :: Player -> World -> ByteString
blockMsg_ p world =
  toStrict $
    runBitPut
      ( do
          putUpdateMask_ (updateMask p)
          let playerBlockMsg = playerBlockSet_ p world
           in do
                putNBits 8 $ toWord_ $ fromIntegral (- ByteString.length playerBlockMsg)
                putByteString playerBlockMsg
      )

putUpdateMask_ :: PlayerUpdateMask -> BitPut
putUpdateMask_ 0 = putNBits 0 $ toWord_ 0
putUpdateMask_ mask =
  if mask >= 256
    then putNBits 16 $ toShortLE_ (fromIntegral (mask .|. 64))
    else putNBits 8 mask

playerBlockSet_ :: Player -> World -> ByteString
playerBlockSet_ player world = do
  ByteString.concat $
    map
      (\x -> x (updateMask player) player world)
      [ addBlockIfRequired appearanceFlag appearanceBlock_,
        addBlockIfRequired chatFlag encodeChatUpdateBlock
      ]

-- [if (mask .&. appearanceBlock_) then appearanceBlock_]

appearanceBlock_ :: Player -> World -> ByteString
appearanceBlock_ player world =
  toStrict $
    runBitPut
      ( do
          putNBits 8 $ toWord_ 0 -- gender
          -- putNBits 8 $ toWord_ (-1) -- prayer
          putNBits 8 $ toWord_ (-1) -- skull icon
          -- no player transformId

          -- equipment
          putNBits 16 $ toShort_ (512 + 0) -- Helmet model
          putNBits 16 $ toShort_ (512 + 0) -- Cape Model
          putNBits 16 $ toShort_ (512 + 0) -- Amulet Model
          putNBits 16 $ toShort_ (512 + 0) -- Weapon Model

          -- chest model
          putNBits 16 $ toShort_ (256 + 19) -- empty chest, appearance code 19
          putNBits 16 $ toShort_ (512 + 0) -- shield
          putNBits 16 $ toShort_ (256 + 27) -- not full armour, so showing arms, appearance code 27
          putNBits 16 $ toShort_ (256 + 37) -- showing legs, appearance code 37
          putNBits 16 $ toShort_ (256 + 0) -- no full helmet, so showing head, appearance code 0
          putNBits 16 $ toShort_ (256 + 33) -- no gloves, so showing hands, appearance code 33
          putNBits 16 $ toShort_ (256 + 42) -- no boots, so showing hands, appearance code 42
          putNBits 16 $ toShort_ (256 + 10) -- male, so showing beard, appearance code 10

          -- model colours
          putNBits 8 $ toWord_ 0 -- hair
          putNBits 8 $ toWord_ 0 -- torso
          putNBits 8 $ toWord_ 0 -- leg
          putNBits 8 $ toWord_ 0 -- feet
          putNBits 8 $ toWord_ 0 -- skin

          -- model animations
          putNBits 16 $ toShort_ 808 -- standing
          putNBits 16 $ toShort_ 823 -- standing turn
          putNBits 16 $ toShort_ 819 -- walking
          putNBits 16 $ toShort_ 820 -- turn 180
          putNBits 16 $ toShort_ 821 -- turn 90 CW
          putNBits 16 $ toShort_ 822 -- turn 90 CCW
          putNBits 16 $ toShort_ 824 -- running
          putNBits 64 $ encodeToBase37 $ P.username player -- username hash
          putNBits 8 $ toWord_ 3 -- combat level
          putNBits 16 $ toShort_ 0 -- skill level for games room
      )
