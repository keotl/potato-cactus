module PotatoCactus.Network.Packets.Out.UpdateFriendsListStatusPacket where

import Data.Binary (Word8)
import Data.Binary.BitPut (putNBits, runBitPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Game.Interface.FriendsList (FriendsListStatus (Connecting, Loaded, Loading))
import PotatoCactus.Network.Binary (toWord_)
import PotatoCactus.Network.Packets.Packet (fixedPacket)

updateFriendsListStatusPacket :: FriendsListStatus -> ByteString
updateFriendsListStatusPacket status =
  fixedPacket
    221
    ( do
        putNBits 8 $ mapStatus_ status
    )

mapStatus_ :: FriendsListStatus -> Word8
mapStatus_ Loading = 0
mapStatus_ Connecting = 1
mapStatus_ Loaded = 2
