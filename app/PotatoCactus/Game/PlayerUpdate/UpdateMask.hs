module PotatoCactus.Game.PlayerUpdate.UpdateMask where

import Data.Word (Word16)

type PlayerUpdateMask = Word16

appearanceFlag :: Word16
appearanceFlag = 0x10

facePositionFlag :: Word16
facePositionFlag = 0x02

animationFlag :: Word16
animationFlag = 8

chatFlag :: Word16
chatFlag = 128

forcedChatFlag :: Word16
forcedChatFlag = 4

forcedMovementFlag :: Word16
forcedMovementFlag = 256

graphicUpdateFlag :: Word16
graphicUpdateFlag = 256

primaryHealthUpdateFlag :: Word16
primaryHealthUpdateFlag = 0x20

secondaryHealthUpdateFlag :: Word16
secondaryHealthUpdateFlag = 0x200
