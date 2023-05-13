{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.ActionResultMapper (mapResult) where

import Data.Aeson (FromJSON, Result (Error, Success), Value (Object, String), decode, decodeStrict, (.:), (.:?))
import Data.Aeson.Types (Object, parse)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import PotatoCactus.Game.Entity.Animation.Animation (Animation (Animation), AnimationPriority (High, Low, Normal))
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject (Added, Removed))
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import qualified PotatoCactus.Game.Entity.Object.GameObject as O
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Game.Scripting.Actions.CreateInterface (CreateInterfaceRequest (CreateInterfaceRequest))
import qualified PotatoCactus.Game.Scripting.Actions.CreateInterface as I
import PotatoCactus.Game.Scripting.Actions.ScriptInvocation (ScriptInvocation (ScriptInvocation))
import PotatoCactus.Game.Scripting.Actions.SpawnNpcRequest (SpawnNpcRequest (SpawnNpcRequest))
import PotatoCactus.Game.Scripting.ScriptUpdates (ScriptActionResult (AddGameObject, ClearPlayerInteraction, ClearStandardInterface, CreateInterface, InternalNoop, InternalProcessingComplete, InvokeScript, NpcQueueWalk, NpcSetAnimation, NpcSetForcedChat, SendMessage, ServerPrintMessage, SetPlayerAnimation, SetPlayerEntityData, SetPlayerPosition, SpawnNpc))

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
decodeBody "sendMessage" body =
  case parse
    ( \obj -> do
        playerIndex <- obj .: "playerIndex"
        text <- obj .: "text"
        return (SendMessage playerIndex text)
    )
    body of
    Error msg -> InternalNoop
    Success decoded -> decoded
decodeBody "setPlayerPosition" body =
  case parse
    ( \obj -> do
        playerIndex <- obj .: "playerIndex"
        posObj <- obj .: "position"
        return (SetPlayerPosition playerIndex $ decodePos_ posObj)
    )
    body of
    Error msg -> InternalNoop
    Success decoded -> decoded
decodeBody "setPlayerAnimation" body =
  case parse
    ( \obj -> do
        playerIndex <- obj .: "playerIndex"
        animationId <- obj .: "animationId"
        delay <- obj .: "delay"
        priority <- obj .: "priority"
        return
          ( SetPlayerAnimation
              playerIndex
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
decodeBody "invokeScript" body =
  case parse
    ( \obj -> do
        f <- obj .: "f"
        args <- obj .: "args"
        delay <- obj .: "delay"
        return $ InvokeScript (ScriptInvocation f args) delay
    )
    body of
    Error msg -> trace msg InternalNoop
    Success decoded -> decoded
decodeBody "createInterface" body =
  case parse
    ( \obj -> do
        interfaceType <- obj .: "type"
        playerIndex <- obj .: "playerIndex"
        elements <- obj .: "elements"
        onClose <- obj .:? "onClose"
        callbacks <- obj .: "callbacks"
        return
          ( CreateInterface
              playerIndex
              $ I.CreateInterfaceRequest
                (decodeInterfaceType_ interfaceType)
                ( mapMaybe
                    decodeInterfaceElement_
                    elements
                )
                (decodeScriptInvocation_ onClose)
                callbacks
          )
    )
    body of
    Error msg -> trace msg InternalNoop
    Success decoded -> decoded
decodeBody "clearStandardInterface" body =
  case parse
    ( \obj -> do
        playerIndex <- obj .: "playerIndex"
        return (ClearStandardInterface playerIndex)
    )
    body of
    Error msg -> trace msg InternalNoop
    Success decoded -> decoded
decodeBody "setPlayerEntityData" body =
  case parse
    ( \obj -> do
        playerIndex <- obj .: "playerIndex"
        key <- obj .: "key"
        val <- obj .: "val"
        return
          (SetPlayerEntityData playerIndex key val)
    )
    body of
    Error msg -> trace msg InternalNoop
    Success decoded -> decoded
decodeBody "spawnObject" body =
  case parse
    ( \obj -> do
        objectId <- obj .: "objectId"
        positionObj <- obj .: "position"
        objectType <- obj .: "objectType"
        facingDirection <- obj .: "facingDirection"
        return
          ( AddGameObject $
              Added $
                GameObject objectId (decodePos_ positionObj) objectType facingDirection
          )
    )
    body of
    Error msg -> trace msg InternalNoop
    Success decoded -> decoded
decodeBody "removeObject" body =
  case parse
    ( \obj -> do
        objectId <- obj .: "objectId"
        positionObj <- obj .: "position"
        objectType <- obj .: "objectType"
        facingDirection <- obj .: "facingDirection"
        return
          ( AddGameObject $
              Removed $
                GameObject objectId (decodePos_ positionObj) objectType facingDirection
          )
    )
    body of
    Error msg -> trace msg InternalNoop
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

decodeInterfaceElement_ :: Object -> Maybe I.InterfaceElement
decodeInterfaceElement_ body =
  case parse
    ( \obj -> do
        elementType <- obj .: "type"
        return $ mapInterfaceElement_ elementType body
    )
    body of
    Error msg -> Nothing
    Success decoded -> decoded

mapInterfaceElement_ :: String -> Object -> Maybe I.InterfaceElement
mapInterfaceElement_ "text" body =
  case parse
    ( \obj -> do
        widgetId <- obj .: "widgetId"
        msg <- obj .: "msg"
        return (I.TextElement widgetId msg)
    )
    body of
    Error msg -> Nothing
    Success decoded -> Just decoded
mapInterfaceElement_ "chatboxRoot" body =
  case parse
    ( \obj -> do
        widgetId <- obj .: "widgetId"
        return (I.ChatboxRootWindowElement widgetId)
    )
    body of
    Error msg -> Nothing
    Success decoded -> Just decoded
mapInterfaceElement_ "npcChathead" body =
  case parse
    ( \obj -> do
        widgetId <- obj .: "widgetId"
        npcId <- obj .: "npcId"
        return (I.NpcChatheadElement widgetId npcId)
    )
    body of
    Error msg -> Nothing
    Success decoded -> Just decoded
mapInterfaceElement_ "playerChathead" body =
  case parse
    ( \obj -> do
        widgetId <- obj .: "widgetId"
        return (I.PlayerChatheadElement widgetId)
    )
    body of
    Error msg -> Nothing
    Success decoded -> Just decoded
mapInterfaceElement_ "modelAnimation" body =
  case parse
    ( \obj -> do
        widgetId <- obj .: "widgetId"
        animationId <- obj .: "animationId"
        return (I.ModelAnimationElement widgetId animationId)
    )
    body of
    Error msg -> Nothing
    Success decoded -> Just decoded
mapInterfaceElement_ _ _ = Nothing

decodeScriptInvocation_ :: Maybe Object -> Maybe ScriptInvocation
decodeScriptInvocation_ Nothing = Nothing
decodeScriptInvocation_ (Just body) =
  case parse
    ( \obj -> do
        f <- obj .: "f"
        args <- obj .: "args"
        return (ScriptInvocation f args)
    )
    body of
    Error msg -> Nothing
    Success decoded -> Just decoded

decodeInterfaceType_ :: String -> I.InterfaceType
decodeInterfaceType_ "standard" = I.Standard
decodeInterfaceType_ "walkable" = I.Walkable
decodeInterfaceType_ "input" = I.Input
decodeInterfaceType_ _ = I.Standard
