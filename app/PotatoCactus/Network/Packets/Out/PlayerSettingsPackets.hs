module PotatoCactus.Network.Packets.Out.PlayerSettingsPackets where

import Data.Binary (Word8)
import Data.Binary.BitPut (putByteString, putNBits, runBitPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import PotatoCactus.Game.Interface.PlayerSettings (BrightnessLevel (Brightness0, Brightness1, Brightness2, Brightness3, Brightness4), MouseType (None, OneButton, TwoButtons), PlayerSettings (acceptAid, autoRetaliate, brightnessLevel, chatEffects, effectsVolume, mouseType, musicVolume, running, splitPrivateChat), VolumeLevel (Volume1, Volume2, Volume3, Volume4, VolumeOff))
import PotatoCactus.Network.Binary (toShortLE_)

allPlayerSettingsPackets :: PlayerSettings -> ByteString
allPlayerSettingsPackets settings =
  toStrict $
    runBitPut
      ( do
          putByteString $ setBrightnessSettingPacket (brightnessLevel settings)
          putByteString $ setMouseTypePacket (mouseType settings)
          putByteString $ setShowChatEffectsPacket (chatEffects settings)
          putByteString $ setShowSplitPrivateChatPacket (splitPrivateChat settings)
          putByteString $ setAcceptAidPacket (acceptAid settings)
          putByteString $ setMusicVolumePacket (musicVolume settings)
          putByteString $ setEffectsVolumePacket (effectsVolume settings)
          putByteString $ setRunningPacket (running settings)
          putByteString $ setAutoRetaliatePacket (autoRetaliate settings)
      )

setBrightnessSettingPacket :: BrightnessLevel -> ByteString
setBrightnessSettingPacket level =
  let brightnessValue =
        ( case level of
            Brightness0 -> 0
            Brightness1 -> 1
            Brightness2 -> 2
            Brightness3 -> 3
            Brightness4 -> 4
        )
   in shortConfigPacket 166 brightnessValue

setMouseTypePacket :: MouseType -> ByteString
setMouseTypePacket mouseType =
  let value =
        ( case mouseType of
            None -> 0
            OneButton -> 1
            TwoButtons -> 2
        )
   in shortConfigPacket 170 value

setShowChatEffectsPacket :: Bool -> ByteString
setShowChatEffectsPacket value =
  shortConfigPacket 171 $ mapBool_ (not value)

setShowSplitPrivateChatPacket :: Bool -> ByteString
setShowSplitPrivateChatPacket value =
  shortConfigPacket 287 (mapBool_ value)

setAcceptAidPacket :: Bool -> ByteString
setAcceptAidPacket value =
  shortConfigPacket 427 (mapBool_ value)

setMusicVolumePacket :: VolumeLevel -> ByteString
setMusicVolumePacket level =
  shortConfigPacket 168 (mapVolumeLevel_ level)

setEffectsVolumePacket :: VolumeLevel -> ByteString
setEffectsVolumePacket level =
  shortConfigPacket 169 (mapVolumeLevel_ level)

setRunningPacket :: Bool -> ByteString
setRunningPacket value =
  shortConfigPacket 173 (mapBool_ value)

setAutoRetaliatePacket :: Bool -> ByteString
setAutoRetaliatePacket value =
  shortConfigPacket 172 $ mapBool_ (not value)

mapBool_ :: Bool -> Word8
mapBool_ True = 1
mapBool_ False = 0

mapVolumeLevel_ :: VolumeLevel -> Word8
mapVolumeLevel_ VolumeOff = 4
mapVolumeLevel_ Volume1 = 3
mapVolumeLevel_ Volume2 = 2
mapVolumeLevel_ Volume3 = 1
mapVolumeLevel_ Volume4 = 0

shortConfigPacket :: Int -> Word8 -> ByteString
shortConfigPacket configId value =
  toStrict $
    runBitPut
      ( do
          putNBits 16 $ toShortLE_ configId
          putNBits 8 value
      )
