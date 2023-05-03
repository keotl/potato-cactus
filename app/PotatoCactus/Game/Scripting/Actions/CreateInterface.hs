module PotatoCactus.Game.Scripting.Actions.CreateInterface where

import PotatoCactus.Game.Definitions.NpcDefinitions (NpcDefinitionId)
import PotatoCactus.Game.Entity.Animation.Animation (AnimationId)
import PotatoCactus.Game.Scripting.Actions.ScriptInvocation (ScriptInvocation)

type WidgetId = Int

data InterfaceType = Standard | Chatbox | Walkable deriving (Show)

data CreateInterfaceRequest = CreateInterfaceRequest
  { -- interfaceId :: Int,
    interfaceType :: InterfaceType,
    elements :: [InterfaceElement],
    onClose :: Maybe ScriptInvocation
  }
  deriving (Show)

data InterfaceElement
  = ChatboxRootWindowElement WidgetId
  | TextElement WidgetId String
  | NpcChatheadElement WidgetId NpcDefinitionId
  | ModelAnimationElement WidgetId AnimationId
  deriving (Show)
