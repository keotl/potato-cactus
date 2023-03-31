module PotatoCactus.Network.Packets.Out.NpcUpdate.EncodeNpcAnimationBlock (encodeNpcAnimationBlock) where

import Data.Binary.Put (putWord16le, putWord8, runPut)
import Data.ByteString (ByteString, empty)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Game.Entity.Animation.Animation (Animation (animationId, delay))
import PotatoCactus.Game.Entity.Npc.Npc (Npc (animation))
import PotatoCactus.Game.World (World)

encodeNpcAnimationBlock :: Npc -> World -> ByteString
encodeNpcAnimationBlock npc _ =
  case animation npc of
    Nothing -> empty
    Just anim ->
      toStrict $
        runPut
          ( do
              putWord16le . fromIntegral $ animationId anim
              putWord8 . fromIntegral $ delay anim
          )
