module PotatoCactus.Network.Packets.Out.UpdateSkillPacket where

import Data.Binary.BitPut (putNBits, runBitPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Game.Skills (experienceToLevel)
import qualified PotatoCactus.Game.Skills as S
import PotatoCactus.Network.Binary (toIntME_, toWord_)
import PotatoCactus.Network.Packets.Packet (fixedPacket)

updateSkillPacket :: S.Skill -> ByteString
updateSkillPacket skill =
  fixedPacket
    134
    ( do
        putNBits 8 $ toWord_ $ S.id (S.skill skill)
        putNBits 32 $ toIntME_ $ S.experience skill
        putNBits 8 $ toWord_ $ experienceToLevel (S.experience skill)
    )
