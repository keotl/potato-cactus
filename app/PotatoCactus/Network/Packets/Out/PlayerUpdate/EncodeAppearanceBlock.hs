module PotatoCactus.Network.Packets.Out.PlayerUpdate.EncodeAppearanceBlock where

import Data.Binary.BitPut (putNBits, runBitPut)
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Game.Player (Player (username))
import PotatoCactus.Game.World (World)
import PotatoCactus.Network.Binary (encodeToBase37, toShort_, toWord_)

encodeAppearanceBlock :: Player -> World -> ByteString
encodeAppearanceBlock player world =
  let body =
        toStrict $
          runBitPut
            ( do
                putNBits 8 $ toWord_ 0 -- gender
                -- putNBits 8 $ toWord_ (-1) -- prayer
                putNBits 8 $ toWord_ 0 -- head icon
                -- no player transformId

                -- equipment
                putNBits 16 $ toShort_ (512 + 0) -- Helmet model
                putNBits 16 $ toShort_ (512 + 0) -- Cape Model
                putNBits 16 $ toShort_ (512 + 0) -- Amulet Model
                putNBits 16 $ toShort_ (512 + 0) -- Weapon Model

                -- chest model
                putNBits 16 $ toShort_ (256 + 19) -- empty chest, appearance code 19
                putNBits 16 $ toShort_ (512 + 0) -- shield
                putNBits 16 $ toShort_ (256 + 27) -- not full armour, so showing arms, appearance code 27
                putNBits 16 $ toShort_ (256 + 37) -- showing legs, appearance code 37
                putNBits 16 $ toShort_ (256 + 0) -- no full helmet, so showing head, appearance code 0
                putNBits 16 $ toShort_ (256 + 33) -- no gloves, so showing hands, appearance code 33
                putNBits 16 $ toShort_ (256 + 42) -- no boots, so showing hands, appearance code 42
                putNBits 16 $ toShort_ (256 + 10) -- male, so showing beard, appearance code 10

                -- model colours
                putNBits 8 $ toWord_ 0 -- hair
                putNBits 8 $ toWord_ 0 -- torso
                putNBits 8 $ toWord_ 0 -- leg
                putNBits 8 $ toWord_ 0 -- feet
                putNBits 8 $ toWord_ 0 -- skin

                -- model animations
                putNBits 16 $ toShort_ 808 -- standing
                putNBits 16 $ toShort_ 823 -- standing turn
                putNBits 16 $ toShort_ 819 -- walking
                putNBits 16 $ toShort_ 820 -- turn 180
                putNBits 16 $ toShort_ 821 -- turn 90 CW
                putNBits 16 $ toShort_ 822 -- turn 90 CCW
                putNBits 16 $ toShort_ 824 -- running
                putNBits 64 $ encodeToBase37 $ username player -- username hash
                putNBits 8 $ toWord_ 3 -- combat level
                putNBits 16 $ toShort_ 0 -- skill level for games room
            )
   in ByteString.concat [pack [fromIntegral (- ByteString.length body)], body]
