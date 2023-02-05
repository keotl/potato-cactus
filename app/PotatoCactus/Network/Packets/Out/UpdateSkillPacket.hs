module PotatoCactus.Network.Packets.Out.UpdateSkillPacket where

import Data.Binary.BitPut (putNBits, runBitPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Game.Skills (experienceToLevel)
import qualified PotatoCactus.Game.Skills as S
import PotatoCactus.Network.Binary (toIntME_, toWord_)

updateSkillPacket :: S.Skill -> ByteString
updateSkillPacket skill =
  toStrict $
    runBitPut
      ( do
          putNBits 8 $ toWord_ 134
          putNBits 8 $ toWord_ $ S.id (S.skill skill)
          putNBits 32 $ toIntME_ $ S.experience skill
          putNBits 8 $ toWord_ $ experienceToLevel (S.experience skill)
      )
