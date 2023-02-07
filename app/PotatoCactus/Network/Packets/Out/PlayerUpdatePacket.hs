module PotatoCactus.Network.Packets.Out.PlayerUpdatePacket where

import Data.Binary.BitPut (putNBits)
import Data.ByteString (ByteString)
import PotatoCactus.Game.Player (Player)
import PotatoCactus.Network.Packets.Packet (varShortPacket)
import PotatoCactus.Network.Binary (toWord_)

playerUpdatePacket :: Player -> ByteString
playerUpdatePacket player =
  varShortPacket
    81
    ( do
        -- TODO -   - keotl 2023-02-06
        putNBits 0 $ toWord_ 0
    )
