module PotatoCactus.Game.Interface.PlayerSettings where

data PlayerSettings = PlayerSettings
  { brightnessLevel :: BrightnessLevel,
    mouseType :: MouseType,
    chatEffects :: Bool,
    splitPrivateChat :: Bool,
    acceptAid :: Bool,
    musicVolume :: VolumeLevel,
    effectsVolume :: VolumeLevel,
    running :: Bool,
    autoRetaliate :: Bool
  }

data MouseType = TwoButtons | OneButton

data VolumeLevel = VolumeOff | Volume1 | Volume2 | Volume3 | Volume4

data BrightnessLevel = Brightness0 | Brightness1 | Brightness2 | Brightness3 | Brightness4
