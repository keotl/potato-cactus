module PotatoCactus.Network.Packets.In.PlayerWalkPacket (playerWalkMessage, playerMapWalk) where

import Control.Monad (replicateM)
import Data.Binary (Word8)
import Data.Binary.Get (Get, getInt8, getWord16le, getWord8, runGet)
import Data.Bits (xor)
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (fromStrict)
import Data.Word (Word16)
import PotatoCactus.Game.Movement.PositionXY (PositionXY (PositionXY))
import PotatoCactus.Game.Movement.WalkingStep (WalkingStep (WalkingStep))
import PotatoCactus.Network.Packets.Reader (InboundPacket (payload))
import PotatoCactus.Game.Message.GameChannelMessage (GameChannelMessage (PlayerWalkMessage))

playerWalkMessage :: String -> InboundPacket -> GameChannelMessage
playerWalkMessage username packet =
  let blob = payload packet
   in let pathLength = (ByteString.length blob - 5) `div` 2
       in assembleMessage_ username $ runGet (readPayload_ pathLength) (fromStrict blob)

playerMapWalk :: String -> InboundPacket -> GameChannelMessage
playerMapWalk username packet =
  let blob = payload packet
   in let pathLength = (ByteString.length blob - 5 - 14) `div` 2
       in assembleMessage_ username $ runGet (readPayload_ pathLength) (fromStrict blob)

readPayload_ :: Int -> Get (PositionXY, Bool, [WalkingStep])
readPayload_ pathLength = do
  x <- getWord16le
  path <- replicateM pathLength readStep_
  y <- getWord16le
  isRunrning <- getWord8

  return
    ( PositionXY (fromIntegral (x `xor` 128)) (fromIntegral y),
      (128 - isRunrning) == 1,
      path
    )

readStep_ :: Get WalkingStep
readStep_ = do
  dx <- getInt8
  dy <- getInt8
  return $ WalkingStep (fromIntegral dx) (fromIntegral dy)

assembleMessage_ :: String -> (PositionXY, Bool, [WalkingStep]) -> GameChannelMessage
assembleMessage_ username payload =
  let (initialPos, isRunning, path) = payload
   in PlayerWalkMessage username initialPos isRunning path
