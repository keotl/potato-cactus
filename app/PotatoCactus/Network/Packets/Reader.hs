module PotatoCactus.Network.Packets.Reader where

import Data.ByteString (ByteString, empty)
import Network.Socket (Socket)

data InboundPacket = InboundPacket
  { opcode :: Int,
    payload :: ByteString
  }

readPacket :: Socket -> IO InboundPacket
readPacket sock = do
  return InboundPacket {opcode = 14, payload = empty}
