{-# LANGUAGE BlockArguments #-}

module PotatoCactus.Network.Packets.Out.PlayerUpdate.PlayerUpdatePacket where

import Data.Binary (Word16, Word8)
import Data.Binary.BitPut (BitPut, putBit, putBits, putByteString, putNBits, runBitPut)
import Data.Bits (Bits (complement, xor, (.|.)), (.&.))
import Data.ByteString (ByteString, empty, pack)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as Data.Binary
import Data.List (find)
import GHC.RTS.Flags (ProfFlags (modSelector))
import PotatoCactus.Client.LocalEntityList (LocalEntity (LocalEntity), LocalEntityList, LocalEntityStatus (Added, Removed, Retained))
import PotatoCactus.Game.Player as P (Player (serverIndex, updateMask, username))
import PotatoCactus.Game.PlayerUpdate.UpdateMask (PlayerUpdateMask, appearanceFlag, chatFlag)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (x, y))
import PotatoCactus.Game.World as W
  ( ClientHandle (username),
    World (players),
  )
import PotatoCactus.Network.Binary (encodeToBase37, toByte, toShortLE_, toShort_, toWord_)
import PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeAppearanceBlock (encodeAppearanceBlock)
import PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeBlock (addBlockIfRequired)
import PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeChatUpdate (encodeChatUpdateBlock)
import PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodePlayerMovement (MovementUpdateType (UpdateOther, UpdateSelf), encodePlayerMovement)
import PotatoCactus.Network.Packets.Packet (varShortPacket)

playerUpdatePacket :: Player -> LocalEntityList Player -> World -> ByteString
playerUpdatePacket player localPlayers world =
  varShortPacket
    81
    do
      let myBlockMsg = blockMsg_ (withIgnoredFlag_ chatFlag player) world
       in let blockMsg = ByteString.concat (myBlockMsg : otherPlayerBlocks_ localPlayers world)
           in do
                putByteString $
                  beforeBlockMsg_
                    player
                    localPlayers
                    world
                    (ByteString.length blockMsg > 0)

                putByteString blockMsg

beforeBlockMsg_ :: Player -> LocalEntityList Player -> World -> Bool -> ByteString
beforeBlockMsg_ player localPlayers world hasUpdateBlocks =
  toStrict $
    runBitPut
      ( do
          encodePlayerMovement (withIgnoredFlag_ chatFlag player) UpdateSelf
          putNBits 8 $ toWord_ (clientKnownPlayers_ localPlayers) -- localplayers list length
          otherExistingPlayerMovement_ localPlayers
          addOtherNewPlayers_ player localPlayers
          if hasUpdateBlocks then putNBits 11 $ toShort_ 2047 else putNBits 0 $ toWord_ 0
      )

clientKnownPlayers_ :: LocalEntityList Player -> Int
clientKnownPlayers_ localPlayers =
  length
    ( filter
        ( \(LocalEntity p status) -> case status of
            Added -> False
            _ -> True
        )
        localPlayers
    )

otherExistingPlayerMovement_ :: LocalEntityList Player -> BitPut
otherExistingPlayerMovement_ = mapM_ mapOtherPlayerMovementUpdate_

mapOtherPlayerMovementUpdate_ :: LocalEntity Player -> BitPut
mapOtherPlayerMovementUpdate_ (LocalEntity _ Added) = putNBits 0 $ toWord_ 0
mapOtherPlayerMovementUpdate_ (LocalEntity _ Removed) = do
  putBit True
  putNBits 2 $ toWord_ 3
mapOtherPlayerMovementUpdate_ (LocalEntity p Retained) = encodePlayerMovement p UpdateOther

addOtherNewPlayers_ :: Player -> LocalEntityList Player -> BitPut
addOtherNewPlayers_ refPlayer = mapM_ (mapAddOtherNewPlayer_ refPlayer)

mapAddOtherNewPlayer_ :: Player -> LocalEntity Player -> BitPut
mapAddOtherNewPlayer_ refPlayer (LocalEntity p Added) = do
  putNBits 11 $ toShort_ (serverIndex p)
  putBit True
  putBit True
  let deltaY = (y . getPosition) p - (y . getPosition) refPlayer
   in putNBits 5 $ toWord_ deltaY
  let deltaX = (x . getPosition) p - (x . getPosition) refPlayer
   in putNBits 5 $ toWord_ deltaX
mapAddOtherNewPlayer_ _ (LocalEntity _ _) = putNBits 0 $ toWord_ 0

withIgnoredFlag_ :: Word16 -> Player -> Player
withIgnoredFlag_ flag player =
  player {updateMask = updateMask player .&. complement flag}

withForcedFlag_ :: Word16 -> Player -> Player
withForcedFlag_ flag player =
  player {updateMask = updateMask player .|. flag}

otherPlayerBlocks_ :: LocalEntityList Player -> World -> [ByteString]
otherPlayerBlocks_ otherPlayers world =
  map (`otherPlayerBlock_` world) otherPlayers

otherPlayerBlock_ :: LocalEntity Player -> World -> ByteString
otherPlayerBlock_ (LocalEntity _ Removed) _ = empty
otherPlayerBlock_ (LocalEntity p Added) world =
  blockMsg_ (withForcedFlag_ appearanceFlag p) world
otherPlayerBlock_ (LocalEntity p Retained) world =
  blockMsg_ p world

blockMsg_ :: Player -> World -> ByteString
blockMsg_ p world =
  toStrict $
    runBitPut
      ( do
          putUpdateMask_ (updateMask p)
          let playerBlockMsg = playerBlockSet_ p world
           in do
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
      [ addBlockIfRequired appearanceFlag encodeAppearanceBlock,
        addBlockIfRequired chatFlag encodeChatUpdateBlock
      ]
