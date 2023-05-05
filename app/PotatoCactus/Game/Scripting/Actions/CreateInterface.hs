module PotatoCactus.Game.Scripting.Actions.CreateInterface where

import PotatoCactus.Game.Definitions.NpcDefinitions (NpcDefinitionId)
import PotatoCactus.Game.Entity.Animation.Animation (AnimationId)
import PotatoCactus.Game.Scripting.Actions.ScriptInvocation (ScriptInvocation)
import Data.IntMap (IntMap)

type WidgetId = Int

data InterfaceType = Standard | Input | Walkable deriving (Show)

data CreateInterfaceRequest = CreateInterfaceRequest
  { -- interfaceId :: Int,
    interfaceType :: InterfaceType,
    elements :: [InterfaceElement],
    onClose :: Maybe ScriptInvocation,
    callbacks :: IntMap ScriptInvocation
  }
  deriving (Show)

data InterfaceElement
  = ChatboxRootWindowElement WidgetId
  | TextElement WidgetId String
  | NpcChatheadElement WidgetId NpcDefinitionId
  | PlayerChatheadElement WidgetId
  | ModelAnimationElement WidgetId AnimationId
  deriving (Show)
