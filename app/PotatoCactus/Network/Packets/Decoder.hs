module PotatoCactus.Network.Packets.Decoder where

import PotatoCactus.Network.Packets.In.LoginPacket (LoginPacket)

data GamePacket = LoginPacket

-- decodePacket :: Int -> Socket -> IO (Maybe GamePacket)
