module PotatoCactus.Game.Scripting.Actions.CreateInterface where
import PotatoCactus.Game.Definitions.NpcDefinitions (NpcDefinitionId)
import PotatoCactus.Game.Scripting.Actions.ScriptInvocation (ScriptInvocation)

type WidgetId = Int

data CreateInterfaceRequest = CreateInterfaceRequest
  {
    -- interfaceId :: Int,
    elements :: [InterfaceElement],
    onClose :: Maybe ScriptInvocation
  }
  deriving (Show)

data InterfaceElement
  = ChatboxRootWindowElement WidgetId
  | TextElement WidgetId String
  | NpcChatheadElement WidgetId NpcDefinitionId
  deriving (Show)
