module PotatoCactus.Game.Interface.InterfaceController (InterfaceController (..), create, clearStandardInterfaces, Interface (..), configureInterface, closeInterface) where

import Data.Maybe (catMaybes, mapMaybe)
import PotatoCactus.Game.Scripting.Actions.CreateInterface (CreateInterfaceRequest (CreateInterfaceRequest), InterfaceElement, InterfaceType (Chatbox, Standard, Walkable))
import PotatoCactus.Game.Scripting.Actions.ScriptInvocation (ScriptInvocation)
import PotatoCactus.Game.Typing (Advance, advance)

data InterfaceController = InterfaceController
  { mainInterface :: Maybe Interface, -- main open window. e.g. bank interface
    chatboxInterface :: Maybe Interface,
    walkableInterface :: Maybe Interface, -- interfaces kept open on movement, e.g. Tutorial island progress
    triggeredCallbacks :: [ScriptInvocation]
  }
  deriving (Show)

instance Advance InterfaceController where
  advance ic =
    ic
      { mainInterface = advanceMaybe_ (mainInterface ic),
        chatboxInterface = advanceMaybe_ (chatboxInterface ic),
        walkableInterface = advanceMaybe_ (walkableInterface ic)
      }

advanceMaybe_ :: Maybe Interface -> Maybe Interface
advanceMaybe_ Nothing = Nothing
advanceMaybe_ (Just i) = Just $ advance i

data Interface = Interface
  { configuredElements :: [InterfaceElement], -- Elements to configure on this tick
    onClose :: Maybe ScriptInvocation -- Script to invoke when dismissed gracefully.
  }
  deriving (Show)

instance Advance Interface where
  advance i =
    i
      { configuredElements = []
      }

create :: InterfaceController
create =
  InterfaceController
    { mainInterface = Nothing,
      chatboxInterface = Nothing,
      walkableInterface = Nothing,
      triggeredCallbacks = []
    }

clearStandardInterfaces :: InterfaceController -> InterfaceController
clearStandardInterfaces c =
  c
    { mainInterface = Nothing,
      chatboxInterface = Nothing
    }

configureInterface :: InterfaceController -> CreateInterfaceRequest -> InterfaceController
configureInterface c (CreateInterfaceRequest Standard elements onClose) =
  c
    { mainInterface = Just $ Interface elements onClose
    }
configureInterface c (CreateInterfaceRequest Chatbox elements onClose) =
  c
    { chatboxInterface = Just $ Interface elements onClose
    }
configureInterface c (CreateInterfaceRequest Walkable elements onClose) =
  c
    { walkableInterface = Just $ Interface elements onClose
    }

closeInterface :: InterfaceController -> InterfaceType -> InterfaceController
closeInterface c Standard =
  enqueueCallback_ (c {mainInterface = Nothing}) (mainInterface c)
closeInterface c Walkable =
  enqueueCallback_ (c {walkableInterface = Nothing}) (walkableInterface c)
closeInterface c Chatbox =
  enqueueCallback_ (c {chatboxInterface = Nothing}) (chatboxInterface c)

enqueueCallback_ :: InterfaceController -> Maybe Interface -> InterfaceController
enqueueCallback_ c Nothing = c
enqueueCallback_ c (Just Interface {onClose = Nothing}) = c
enqueueCallback_ c (Just Interface {onClose = Just script}) =
  c
    { triggeredCallbacks = script : triggeredCallbacks c
    }
