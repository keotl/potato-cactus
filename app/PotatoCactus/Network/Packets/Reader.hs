{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Network.Packets.Reader where

import Data.ByteString (ByteString, empty)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import PotatoCactus.Network.Binary (toByte, toShort)
import PotatoCactus.Network.Packets.PacketLengths (packetSizes)

socketClosedOpcode = -2

data InboundPacket = InboundPacket
  { opcode :: Int,
    payload :: ByteString
  }

readPacket :: Socket -> IO InboundPacket
readPacket sock = do
  opcode <- recv sock 1
  if opcode == ""
    then return (InboundPacket socketClosedOpcode empty)
    else do
      let decodedOpcode = fromIntegral (toByte opcode)
      let predefinedSize = packetSizes !! decodedOpcode
      payload <- readDynamicPayload_ sock predefinedSize
      return $ InboundPacket decodedOpcode payload

readDynamicPayload_ :: Socket -> Integer -> IO ByteString
readDynamicPayload_ sock predefinedSize = do
  case predefinedSize of
    0 -> return empty
    -1 -> do
      size <- recv sock 1
      recv sock $ fromIntegral $ toByte size
    -2 -> do
      size <- recv sock 2
      recv sock $ fromIntegral $ toShort size
    fixed -> do
      recv sock $ fromIntegral fixed
