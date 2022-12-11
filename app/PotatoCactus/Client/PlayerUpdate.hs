{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Client.PlayerUpdate where

import Data.Binary (Word8)
import Data.Binary.BitPut (BitPut, putBit, putBits, putByteString, putNBits, runBitPut)
-- import Data.ByteString
import qualified Data.ByteString.Internal as BSI
import Data.List (find)
import PotatoCactus.Game.Player as P (Player (username))
import PotatoCactus.Game.World as W
  ( ClientHandle (username),
    World (players),
  )

playerUpdate :: ClientHandle -> World -> BitPut
playerUpdate client world = do
  putNBits 8 (toWord_ 81)

  case find (\x -> P.username x == W.username client) (W.players world) of
    Just p -> do
      playerMovement_ p
      playerBlockSet_ p world
    Nothing -> putNBits 0 $ toWord_ 0

playerMovement_ :: Player -> BitPut
playerMovement_ player = do
  putBit True -- needs updating, direction = None
  putNBits 2 (toWord_ 0)

toWord_ :: Int -> Word8
toWord_ = fromIntegral

playerBlockSet_ :: Player -> World -> BitPut
playerBlockSet_ player world = do
  -- todo PlayerUpdateBlockSet
  -- only appearance block is flagged on first connection
  putNBits 0 $ toWord_ 0
