module PotatoCactus.Game.Interface.InterfaceController where

import PotatoCactus.Game.Scripting.Actions.ScriptInvocation (ScriptInvocation)

data InterfaceController = InterfaceController
  { mainInterface :: Maybe Interface, -- main open window. e.g. bank interface
    chatboxInterface :: Maybe Interface,
    walkableInterface :: Maybe Interface -- interfaces kept open on movement, e.g. Tutorial island progress
  }
  deriving (Show)

data Interface = Interface
  { interfaceId :: Int,
    onClose :: Maybe ScriptInvocation
  }
  deriving (Show)

create :: InterfaceController
create =
  InterfaceController
    { mainInterface = Nothing,
      chatboxInterface = Nothing,
      walkableInterface = Nothing
    }

clearStandardInterfaces :: InterfaceController -> InterfaceController
clearStandardInterfaces c =
  c
    { mainInterface = Nothing,
      chatboxInterface = Nothing
    }
