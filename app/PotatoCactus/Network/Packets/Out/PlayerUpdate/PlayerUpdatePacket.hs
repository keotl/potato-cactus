{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Network.Packets.Out.PlayerUpdate.PlayerUpdatePacket where

import Data.Binary (Word16, Word8)
import Data.Binary.BitPut (BitPut, putBit, putBits, putByteString, putNBits, runBitPut)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as Data.Binary
import Data.List (find)
import GHC.RTS.Flags (ProfFlags (modSelector))
import PotatoCactus.Game.Player as P (Player (username))
import PotatoCactus.Game.World as W
  ( ClientHandle (username),
    World (players),
  )
import PotatoCactus.Network.Binary (encodeToBase37, toByte, toShort_, toWord_)
import PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodePlayerMovement (encodePlayerMovement)
import PotatoCactus.Network.Packets.Packet (varShortPacket)

playerUpdatePacket :: Player -> World -> ByteString
playerUpdatePacket player world =
  varShortPacket
    81
    do
      putByteString $ beforeBlockMsg_ player
      let blockMsg = runBitPut $ blockMsg_ player world
       in do
            putByteString $ toStrict blockMsg

beforeBlockMsg_ :: Player -> ByteString
beforeBlockMsg_ player =
  toStrict $
    runBitPut
      ( do
          encodePlayerMovement player
          putNBits 8 $ toWord_ 0 -- localplayers list length
          putNBits 11 $ toShort_ 2047 -- should only put 2047 when blockMsg contains something
      )

blockMsg_ :: Player -> World -> BitPut
blockMsg_ p world = do
  let playerBlockMsg = runBitPut $ playerBlockSet_ p world
   in do
        putNBits 8 $ toWord_ 16 -- 16 = only player appearance update mask
        putNBits 8 $ toWord_ $ fromIntegral (- Data.Binary.length playerBlockMsg)
        putByteString $ toStrict playerBlockMsg

playerBlockSet_ :: Player -> World -> BitPut
playerBlockSet_ player world = do
  -- todo PlayerUpdateBlockSet
  -- only appearance block is flagged on first connection

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
