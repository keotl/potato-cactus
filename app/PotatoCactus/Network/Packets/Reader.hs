{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Network.Packets.Reader where

import Data.ByteString (ByteString, empty)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import PotatoCactus.Network.Binary (toByte)

socketClosedOpcode = -2

data InboundPacket = InboundPacket
  { opcode :: Int,
    payload :: ByteString
  }

readPacket :: Socket -> IO InboundPacket
readPacket sock = do
  opcode <- recv sock 1
  -- TODO - how to actually read payload dynamically?  - keotl 2022-11-29
  return case opcode of
    "" -> InboundPacket socketClosedOpcode empty
    x -> case fromIntegral (toByte x) of
      0 -> InboundPacket 0 empty -- idle
      _ -> InboundPacket (-1) empty -- unknown packet
