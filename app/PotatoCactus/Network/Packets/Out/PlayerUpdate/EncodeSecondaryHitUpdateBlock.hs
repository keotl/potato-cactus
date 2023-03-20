module PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeSecondaryHitUpdateBlock where

import Data.Binary.Put (putByteString, putInt8, putWord8, runPut)
import Data.Bits (xor)
import Data.ByteString (ByteString, empty)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity (hitpoints, maxHitpoints), hits)
import PotatoCactus.Game.Combat.Hit (Hit (damage), hitsplatOpcode)
import PotatoCactus.Game.Player (Player (combat))
import PotatoCactus.Game.World (World)

encodeSecondaryHitUpdateBlock :: Player -> World -> ByteString
encodeSecondaryHitUpdateBlock player _ =
  case hits . combat $ player of
    [] -> empty
    [x] -> empty
    (_ : (hit : _)) ->
      toStrict $
        runPut
          ( do
              putWord8 . fromIntegral $ damage hit
              putInt8 . fromIntegral $ 128 - hitsplatOpcode hit
              putInt8 . fromIntegral . hitpoints . combat $ player
              putWord8 . fromIntegral . negate . maxHitpoints . combat $ player
          )
