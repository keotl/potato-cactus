{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Client.PlayerUpdate where

import Data.Binary (Word16, Word8)
import Data.Binary.BitPut (BitPut, putBit, putBits, putByteString, putNBits, runBitPut)
-- import Data.ByteString

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
import PotatoCactus.Network.Binary (encodeToBase37, toByte)
import PotatoCactus.Network.Packets.Packet (varShortPacket)

playerUpdate :: ClientHandle -> World -> ByteString
playerUpdate client world =
  varShortPacket
    81
    let blockMsg = runBitPut $ blockMsg_ client world
     in do
          putNBits 11 $ toShort_ 2047
          putByteString $ toStrict blockMsg

blockMsg_ :: ClientHandle -> World -> BitPut
blockMsg_ client world = do
  case find (\x -> P.username x == W.username client) (W.players world) of
    Just p -> do
      playerMovement_ p
      let playerBlockMsg = runBitPut $ playerBlockSet_ p world
       in do
            putNBits 8 $ toWord_ $ fromIntegral (- Data.Binary.length playerBlockMsg)
            putByteString $ toStrict playerBlockMsg
    -- todo deal with other players
    Nothing -> putNBits 0 $ toWord_ 0

playerMovement_ :: Player -> BitPut
playerMovement_ player = do
  putBit True -- needs updating, direction = None
  putNBits 2 (toWord_ 0)

toWord_ :: Int -> Word8
toWord_ = fromIntegral

toShort_ :: Int -> Word16
toShort_ = fromIntegral

playerBlockSet_ :: Player -> World -> BitPut
playerBlockSet_ player world = do
  -- todo PlayerUpdateBlockSet
  -- only appearance block is flagged on first connection

  putNBits 8 $ toWord_ 16 -- 16 = player appearance update mask
  putNBits 8 $ toWord_ 0 -- gender
  putNBits 8 $ toWord_ (-1) -- prayer
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
