module PotatoCactus.Network.Packets.Out.SetPlacementReferencePacket where

import Data.Binary.Put (putByteString, putInt8, putWord8)
import Data.ByteString (ByteString, empty)
import PotatoCactus.Game.Movement.PlayerMovement (lastRegionUpdate_)
import PotatoCactus.Game.Player (Player (Player, movement))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (x), chunkX, chunkY, localToRefX, localToRefY, localX, localY)
import PotatoCactus.Network.Packets.Packet (fixedPacket2)

-- localPlayerX and localPlayerY used for placing game objects, ground entities, etc.
setPlacementReferencePacket :: Player -> Position -> ByteString
setPlacementReferencePacket player placement =
  fixedPacket2
    85
    ( do
        case movement player of
          m -> do
            putInt8 $ fromIntegral (- (localToRefY (lastRegionUpdate_ m) placement))
            putInt8 $ fromIntegral (- (localToRefX (lastRegionUpdate_ m) placement))
            -- putInt8 $ fromIntegral (- (localX . lastRegionUpdate_  $ m))
            -- putInt8 $ fromIntegral (- (localY . position_  $ m))
            -- putInt8 $ fromIntegral (- (localX . position_  $ m))
            -- _ -> pure ()
            -- putInt8 $ fromIntegral (- (localY . lastRegionUpdate_ $ m))
            -- putInt8 $ fromIntegral (- (localX . lastRegionUpdate_ $ m))
    )
