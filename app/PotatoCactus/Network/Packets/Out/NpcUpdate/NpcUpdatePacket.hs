{-# LANGUAGE BlockArguments #-}

module PotatoCactus.Network.Packets.Out.NpcUpdate.NpcUpdatePacket (npcUpdatePacket) where

import Data.Binary (Word16, Word8)
import Data.Binary.BitPut (BitPut, putBit, putBits, putByteString, putNBits, runBitPut)
import Data.Bits (Bits (complement, xor, (.|.)), (.&.))
import Data.ByteString (ByteString, empty, pack)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as Data.Binary
import Data.List (find)
import PotatoCactus.Client.LocalEntityList (LocalEntity (LocalEntity), LocalEntityList, LocalEntityStatus (Added, Removed, Retained), clientKnownEntities)
import PotatoCactus.Game.Entity.Npc.Npc (Npc (Npc, definitionId, serverIndex, updateMask))
import PotatoCactus.Game.Entity.Npc.NpcUpdateMask (NpcUpdateMask, npcPrimaryHealthUpdateFlag, npcSecondaryHealthUpdateFlag)
import PotatoCactus.Game.Player (Player)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (x, y))
import PotatoCactus.Game.World as W
  ( ClientHandle (username),
    World (World),
  )
import PotatoCactus.Network.Binary (encodeToBase37, toByte, toShortLE_, toShort_, toWord_)
import PotatoCactus.Network.Packets.Out.NpcUpdate.EncodeNpcMovement (encodeNpcMovement)
import PotatoCactus.Network.Packets.Out.NpcUpdate.EncodePrimaryNpcHitUpdateBlock (encodePrimaryNpcHitUpdateBlock)
import PotatoCactus.Network.Packets.Out.NpcUpdate.EncodeSecondaryNpcHitUpdateBlock (encodeSecondaryNpcHitUpdateBlock)
import PotatoCactus.Network.Packets.Packet (varShortPacket)

npcUpdatePacket :: Player -> LocalEntityList Npc -> World -> ByteString
npcUpdatePacket refPlayer localNpcs world =
  varShortPacket
    65
    do
      let blockMsg = ByteString.concat $ otherNpcBlocks_ localNpcs world
       in do
            putByteString $
              beforeBlockMsg_
                refPlayer
                localNpcs
                world
                (ByteString.length blockMsg > 0)

            putByteString blockMsg

beforeBlockMsg_ :: Player -> LocalEntityList Npc -> World -> Bool -> ByteString
beforeBlockMsg_ player localEntities world hasUpdateBlocks =
  toStrict $
    runBitPut
      ( do
          putNBits 8 $ toWord_ (clientKnownEntities localEntities)
          otherExistingEntityMovement_ localEntities
          addOtherNewEntities_ player localEntities
          if hasUpdateBlocks then putNBits 14 $ toShort_ 16383 else putNBits 0 $ toWord_ 0
      )

otherExistingEntityMovement_ :: LocalEntityList Npc -> BitPut
otherExistingEntityMovement_ = mapM_ mapOtherEntityMovementUpdate_

mapOtherEntityMovementUpdate_ :: LocalEntity Npc -> BitPut
mapOtherEntityMovementUpdate_ (LocalEntity _ Added) = putNBits 0 $ toWord_ 0
mapOtherEntityMovementUpdate_ (LocalEntity _ Removed) = do
  putBit True
  putNBits 2 $ toWord_ 3
mapOtherEntityMovementUpdate_ (LocalEntity e Retained) = encodeNpcMovement e

addOtherNewEntities_ :: Player -> LocalEntityList Npc -> BitPut
addOtherNewEntities_ refPlayer = mapM_ (mapAddOtherNewEntity_ refPlayer)

mapAddOtherNewEntity_ :: Player -> LocalEntity Npc -> BitPut
mapAddOtherNewEntity_ refPlayer (LocalEntity npc Added) = do
  putNBits 14 $ toShort_ (serverIndex npc)
  let deltaY = (y . getPosition) npc - (y . getPosition) refPlayer
   in putNBits 5 $ toWord_ deltaY
  let deltaX = (x . getPosition) npc - (x . getPosition) refPlayer
   in putNBits 5 $ toWord_ deltaX
  putBit (updateMask npc > 0) -- updateRequired
  putNBits 12 $ definitionId npc
  putBit True
mapAddOtherNewEntity_ _ (LocalEntity _ _) = putNBits 0 $ toWord_ 0

withForcedFlag_ :: Word16 -> Npc -> Npc
withForcedFlag_ flag npc =
  npc {updateMask = updateMask npc .|. flag}

otherNpcBlocks_ :: LocalEntityList Npc -> World -> [ByteString]
otherNpcBlocks_ otherPlayers world =
  map (`otherNpcBlock_` world) otherPlayers

otherNpcBlock_ :: LocalEntity Npc -> World -> ByteString
otherNpcBlock_ (LocalEntity _ Removed) _ = empty
otherNpcBlock_ (LocalEntity p Added) world =
  blockMsg_ p world
otherNpcBlock_ (LocalEntity p Retained) world =
  if updateMask p > 0
    then blockMsg_ p world
    else empty

blockMsg_ :: Npc -> World -> ByteString
blockMsg_ p world =
  toStrict $
    runBitPut
      ( do
          putUpdateMask_ (updateMask p)
          let npcBlockMsg = npcBlockSet_ p world
           in do
                putByteString npcBlockMsg
      )

putUpdateMask_ :: NpcUpdateMask -> BitPut
putUpdateMask_ mask =
  if mask >= 256
    then putNBits 16 $ toShortLE_ (fromIntegral (mask .|. 64))
    else putNBits 8 mask

npcBlockSet_ :: Npc -> World -> ByteString
npcBlockSet_ npc world = do
  ByteString.concat $
    map
      (\x -> x (updateMask npc) npc world)
      [ addBlockIfRequired npcPrimaryHealthUpdateFlag encodePrimaryNpcHitUpdateBlock,
        addBlockIfRequired npcSecondaryHealthUpdateFlag encodeSecondaryNpcHitUpdateBlock
      ]

-- [ addBlockIfRequired appearanceFlag encodeAppearanceBlock,
--   addBlockIfRequired chatFlag encodeChatUpdateBlock
-- ]

addBlockIfRequired :: Word16 -> (Npc -> World -> ByteString) -> (Word16 -> (Npc -> World -> ByteString))
addBlockIfRequired updateFlag createBlock mask =
  if (mask .&. updateFlag) > 0
    then createBlock
    else empty_

empty_ :: Npc -> World -> ByteString
empty_ _ _ = empty
