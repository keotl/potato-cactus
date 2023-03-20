module PotatoCactus.Network.Packets.Out.NpcUpdate.EncodeSecondaryNpcHitUpdateBlock where

import Data.Binary.Put (putByteString, putInt8, putWord8, runPut)
import Data.Bits (xor)
import Data.ByteString (ByteString, empty)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Game.Combat.CombatEntity (CombatEntity (hitpoints, maxHitpoints), hits)
import PotatoCactus.Game.Combat.Hit (Hit (damage), hitsplatOpcode)
import PotatoCactus.Game.Entity.Npc.Npc (Npc (combat))
import PotatoCactus.Game.World (World)

encodeSecondaryNpcHitUpdateBlock :: Npc -> World -> ByteString
encodeSecondaryNpcHitUpdateBlock npc _ =
  case hits . combat $ npc of
    [] -> empty
    [x] -> empty
    (_ : (hit : _)) ->
      toStrict $
        runPut
          ( do
              putWord8 . fromIntegral $ 128 `xor` damage hit
              putInt8 . fromIntegral . negate . hitsplatOpcode $ hit
              putWord8 . fromIntegral $ 128 `xor` (hitpoints . combat $ npc)
              putWord8 . fromIntegral . maxHitpoints . combat $ npc
          )
