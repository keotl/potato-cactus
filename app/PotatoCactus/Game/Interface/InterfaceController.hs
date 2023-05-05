module PotatoCactus.Game.Interface.InterfaceController (InterfaceController (..), create, clearStandardInterfaces, Interface (..), configureInterface, closeInterface, dispatchButtonClick) where

import Data.IntMap (IntMap, (!?))
import Data.Maybe (catMaybes, mapMaybe)
import PotatoCactus.Game.Scripting.Actions.CreateInterface (CreateInterfaceRequest (CreateInterfaceRequest), InterfaceElement, InterfaceType (Input, Standard, Walkable))
import PotatoCactus.Game.Scripting.Actions.ScriptInvocation (ScriptInvocation)
import PotatoCactus.Game.Typing (Advance, advance)
import PotatoCactus.Utils.Flow ((|>))

data InterfaceController = InterfaceController
  { mainInterface :: Maybe Interface, -- main open window. e.g. bank interface
    inputInterface :: Maybe Interface,
    walkableInterface :: Maybe Interface, -- interfaces kept open on movement, e.g. Tutorial island progress
    triggeredCallbacks :: [ScriptInvocation],
    pendingCallbacks_ :: [ScriptInvocation],
    shouldCloseInterfaces :: Bool,
    pendingClosingInterfaces_ :: Bool
    -- TODO - shouldClose for walkable interface  - keotl 2023-05-03
  }
  deriving (Show)

instance Advance InterfaceController where
  advance ic =
    ic
      { mainInterface = advanceMaybe_ (mainInterface ic),
        inputInterface = advanceMaybe_ (inputInterface ic),
        walkableInterface = advanceMaybe_ (walkableInterface ic),
        triggeredCallbacks = pendingCallbacks_ ic,
        pendingCallbacks_ = [],
        shouldCloseInterfaces = pendingClosingInterfaces_ ic,
        pendingClosingInterfaces_ = False
      }

advanceMaybe_ :: Maybe Interface -> Maybe Interface
advanceMaybe_ Nothing = Nothing
advanceMaybe_ (Just i) = Just $ advance i

data Interface = Interface
  { configuredElements :: [InterfaceElement], -- Elements to configure on this tick
    onClose :: Maybe ScriptInvocation, -- Script to invoke when dismissed gracefully.
    callbacks :: IntMap ScriptInvocation
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
      inputInterface = Nothing,
      walkableInterface = Nothing,
      triggeredCallbacks = [],
      pendingCallbacks_ = [],
      shouldCloseInterfaces = False,
      pendingClosingInterfaces_ = True
    }

clearStandardInterfaces :: InterfaceController -> InterfaceController
clearStandardInterfaces c =
  c
    { mainInterface = Nothing,
      inputInterface = Nothing,
      pendingClosingInterfaces_ = True
    }

configureInterface :: InterfaceController -> CreateInterfaceRequest -> InterfaceController
configureInterface c (CreateInterfaceRequest Standard elements onClose callbacks) =
  c
    { mainInterface = Just $ Interface elements onClose callbacks,
      shouldCloseInterfaces = False,
      pendingClosingInterfaces_ = False
    }
configureInterface c (CreateInterfaceRequest Input elements onClose callbacks) =
  c
    { inputInterface = Just $ Interface elements onClose callbacks,
      shouldCloseInterfaces = False,
      pendingClosingInterfaces_ = False
    }
configureInterface c (CreateInterfaceRequest Walkable elements onClose callbacks) =
  c
    { walkableInterface = Just $ Interface elements onClose callbacks,
      shouldCloseInterfaces = False,
      pendingClosingInterfaces_ = False
    }

closeInterface :: InterfaceController -> InterfaceType -> InterfaceController
closeInterface c Standard =
  c
    |> (\x -> enqueueOnCloseCallback_ x (mainInterface x))
    |> (\x -> x {pendingClosingInterfaces_ = True, mainInterface = Nothing})
closeInterface c Walkable =
  c
    |> (\x -> enqueueOnCloseCallback_ x (walkableInterface x))
    |> (\x -> x {walkableInterface = Nothing})
closeInterface c Input =
  c
    |> (\x -> enqueueOnCloseCallback_ x (inputInterface x))
    |> (\x -> x {pendingClosingInterfaces_ = True, inputInterface = Nothing})

enqueueOnCloseCallback_ :: InterfaceController -> Maybe Interface -> InterfaceController
enqueueOnCloseCallback_ c Nothing = c
enqueueOnCloseCallback_ c (Just Interface {onClose = Nothing}) = c
enqueueOnCloseCallback_ c (Just Interface {onClose = Just script}) =
  c
    { pendingCallbacks_ = script : pendingCallbacks_ c
    }

dispatchButtonClick :: InterfaceController -> Int -> InterfaceController
dispatchButtonClick c buttonId =
  let invoked =
        [ invokedCallback_ (mainInterface c) buttonId,
          invokedCallback_ (inputInterface c) buttonId,
          invokedCallback_ (walkableInterface c) buttonId
        ]
   in c {pendingCallbacks_ = pendingCallbacks_ c ++ catMaybes invoked}

invokedCallback_ :: Maybe Interface -> Int -> Maybe ScriptInvocation
invokedCallback_ Nothing _ = Nothing
invokedCallback_ (Just Interface {callbacks = c}) buttonId =
  c !? buttonId
