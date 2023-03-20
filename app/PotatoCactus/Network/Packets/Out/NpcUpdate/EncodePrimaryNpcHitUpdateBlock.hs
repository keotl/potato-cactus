module PotatoCactus.Network.Packets.Out.NpcUpdate.EncodePrimaryNpcHitUpdateBlock where

import Data.Binary.Put (putByteString, putInt8, runPut)
import Data.Bits (xor)
import Data.ByteString (ByteString, empty)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity (hitpoints, maxHitpoints), hits)
import PotatoCactus.Game.Combat.Hit (Hit (damage), hitsplatOpcode)
import PotatoCactus.Game.World (World)
import PotatoCactus.Game.Entity.Npc.Npc (Npc (combat))

encodePrimaryNpcHitUpdateBlock :: Npc -> World -> ByteString
encodePrimaryNpcHitUpdateBlock npc _ =
  case hits . combat $ npc of
    [] -> empty
    (hit : _) ->
      toStrict $
        runPut
          ( do
              putInt8 . fromIntegral . negate . damage $ hit
              putInt8 . fromIntegral $ 128 - hitsplatOpcode hit
              putInt8 . fromIntegral $ 128 - (hitpoints . combat $ npc)
              putInt8 . fromIntegral . negate . maxHitpoints . combat $ npc
          )
