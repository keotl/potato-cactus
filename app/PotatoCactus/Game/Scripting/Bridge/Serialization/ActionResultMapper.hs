{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.ActionResultMapper (mapResult) where

import Data.Aeson (FromJSON, Result (Error, Success), Value (Object, String), decode, decodeStrict, (.:))
import Data.Aeson.Types (Object, parse)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Generics (Generic)
import PotatoCactus.Game.Entity.Animation.Animation (Animation (Animation), AnimationPriority (High, Low, Normal))
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject (Added, Removed))
import qualified PotatoCactus.Game.Entity.Object.GameObject as O
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Game.Scripting.Actions.SpawnNpcRequest (SpawnNpcRequest (SpawnNpcRequest))
import PotatoCactus.Game.Scripting.ScriptUpdates (ScriptActionResult (AddGameObject, ClearPlayerInteraction, InternalNoop, InternalProcessingComplete, NpcQueueWalk, NpcSetAnimation, NpcSetForcedChat, SpawnNpc, ServerPrintMessage))

mapResult :: ByteString -> ScriptActionResult
mapResult bytes =
  case decodeStrict bytes :: Maybe DecodedMessage of
    Just val -> decodeBody (op val) (body val)
    Nothing -> InternalNoop

decodeBody :: String -> Object -> ScriptActionResult
decodeBody "internal_processingComplete" _ =
  InternalProcessingComplete
decodeBody "clearPlayerInteraction" body =
  case parse
    ( \obj -> do
        playerIndex <- obj .: "playerIndex"
        return (ClearPlayerInteraction playerIndex)
    )
    body of
    Error msg -> InternalNoop
    Success decoded -> decoded
decodeBody "npcQueueWalk" body =
  case parse
    ( \obj -> do
        npcIndex <- obj .: "npcIndex"
        posObject <- obj .: "position"
        return (NpcQueueWalk npcIndex (decodePos_ posObject))
    )
    body of
    Error msg -> InternalNoop
    Success decoded -> decoded
decodeBody "addGameObject" body =
  case parse
    ( \obj -> do
        op <- obj .: "op"
        objectId <- obj .: "id"
        posObject <- obj .: "position"
        objType <- obj .: "objectType"
        facingDirection <- obj .: "facingDirection"
        return
          ( if op == String "add"
              then
                AddGameObject $
                  Added
                    ( O.GameObject
                        { O.id = objectId,
                          O.position = decodePos_ posObject,
                          O.objectType = objType,
                          O.facingDirection = facingDirection
                        }
                    )
              else
                AddGameObject $
                  Removed
                    ( O.GameObject
                        { O.id = objectId,
                          O.position = decodePos_ posObject,
                          O.objectType = objType,
                          O.facingDirection = facingDirection
                        }
                    )
          )
    )
    body of
    Error msg -> InternalNoop
    Success decoded -> decoded
decodeBody "spawnNpc" body =
  case parse
    ( \obj -> do
        npcId <- obj .: "npcId"
        posObj <- obj .: "position"
        respawnDelay <- obj .: "respawnDelay"
        return (SpawnNpc $ SpawnNpcRequest npcId (decodePos_ posObj) respawnDelay)
    )
    body of
    Error msg -> InternalNoop
    Success decoded -> decoded
decodeBody "npcSetAnimation" body =
  case parse
    ( \obj -> do
        npcIndex <- obj .: "npcIndex"
        animationId <- obj .: "animationId"
        delay <- obj .: "delay"
        priority <- obj .: "priority"
        return
          ( NpcSetAnimation
              npcIndex
              ( Animation
                  animationId
                  delay
                  (decodeAnimationPriority_ priority)
              )
          )
    )
    body of
    Error msg -> InternalNoop
    Success decoded -> decoded
decodeBody "npcSetForcedChat" body =
  case parse
    ( \obj -> do
        npcIndex <- obj .: "npcIndex"
        message <- obj .: "message"
        return
          ( NpcSetForcedChat
              npcIndex
              message
          )
    )
    body of
    Error msg -> InternalNoop
    Success decoded -> decoded
decodeBody "serverPrintMessage" body =
  case parse
    ( \obj -> do
        msg <- obj .: "msg"
        return (ServerPrintMessage msg)
    )
    body of
    Error msg -> InternalNoop
    Success decoded -> decoded
decodeBody _ _ = InternalNoop

data DecodedMessage = DecodedMessage
  { op :: String,
    body :: Object
  }
  deriving (Generic, FromJSON)

decodePos_ :: Object -> Position
decodePos_ body =
  case parse
    ( \obj -> do
        x <- obj .: "x"
        y <- obj .: "y"
        z <- obj .: "z"
        return (Position x y z)
    )
    body of
    Error msg -> Position 0 0 0
    Success decoded -> decoded

decodeAnimationPriority_ :: String -> AnimationPriority
decodeAnimationPriority_ "high" = High
decodeAnimationPriority_ "low" = Low
decodeAnimationPriority_ _ = Normal
