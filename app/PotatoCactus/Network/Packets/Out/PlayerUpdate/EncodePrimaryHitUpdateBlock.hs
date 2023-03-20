module PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodePrimaryHitUpdateBlock where

import Data.Binary.Put (putByteString, putInt8, putWord8, runPut)
import Data.Bits (xor)
import Data.ByteString (ByteString, empty)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity (hitpoints, maxHitpoints), hits)
import PotatoCactus.Game.Combat.Hit (Hit (damage), hitsplatOpcode)
import PotatoCactus.Game.Player (Player (combat))
import PotatoCactus.Game.World (World)

encodePrimaryHitUpdateBlock :: Player -> World -> ByteString
encodePrimaryHitUpdateBlock player _ =
  case hits . combat $ player of
    [] -> empty
    (hit : _) ->
      toStrict $
        runPut
          ( do
              putWord8 . fromIntegral $ damage hit
              putWord8 $ hitsplatOpcode hit `xor` 128
              putInt8 . fromIntegral . negate . hitpoints . combat $ player
              putWord8 . fromIntegral . maxHitpoints . combat $ player
          )
