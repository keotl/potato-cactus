module PotatoCactus.Network.Packets.Out.RemoveObjectPacket where

import Data.Binary.BitPut (putNBits)
import Data.Binary.Put (putWord8)
import Data.ByteString (ByteString)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position, localToRefX, localToRefY)
import PotatoCactus.Network.Binary (toWord_)
import PotatoCactus.Network.Packets.Packet (fixedPacket)

removeObjectPacket :: Position -> GameObject -> ByteString
removeObjectPacket refPos object =
  fixedPacket
    101
    ( do
        -- putNBits 6 $ toWord_ 0 -- type
        -- putNBits 2 $ toWord_ 0 -- facing direction
        putNBits 8 $ toWord_ (-(10 * 4 + 0))
        -- putNBits 4 . toWord_ $ localToRefX refPos (getPosition object)
        putNBits 4 . toWord_ $ 1
        putNBits 4 . toWord_ $ 1
        -- putNBits 4 . toWord_ $ localToRefY refPos (getPosition object)
    )
