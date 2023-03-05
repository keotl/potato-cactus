module PotatoCactus.Network.Packets.Out.AddObjectPacket where

import Data.Binary.BitPut (putNBits)
import Data.ByteString (ByteString)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (id))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position, localToRefX, localToRefY)
import PotatoCactus.Network.Binary (toShortLE_, toShort_, toWord_)
import PotatoCactus.Network.Packets.Packet (fixedPacket)
import Prelude hiding (id)

addObjectPacket :: Position -> GameObject -> ByteString
addObjectPacket refPos object =
  fixedPacket
    151
    ( do
        putNBits 4 . toWord_ $ localToRefX refPos (getPosition object)
        putNBits 4 . toWord_ $ localToRefY refPos (getPosition object)
        putNBits 16 . toShortLE_ . id $ object
        putNBits 6 . toWord_ $ 0 -- type
        putNBits 2 . toWord_ $ 0 -- orientation
    )
