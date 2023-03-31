module PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeAnimationBlock (encodeAnimationBlock) where

import Data.Binary.Put (putInt8, putWord16le, runPut)
import Data.ByteString (ByteString, empty)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Game.Entity.Animation.Animation (Animation (delay), animationId)
import PotatoCactus.Game.Player (Player (animation))
import PotatoCactus.Game.World (World)

encodeAnimationBlock :: Player -> World -> ByteString
encodeAnimationBlock player _ =
  case animation player of
    Nothing -> empty
    Just anim ->
      toStrict $
        runPut
          ( do
              putWord16le . fromIntegral . animationId $ anim
              putInt8 . fromIntegral . negate . delay $ anim
          )
