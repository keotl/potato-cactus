module PotatoCactus.Game.Definitions.StaticGameObjectSet (StaticGameObjectSet, initializeStaticGameSet, staticObjectAt) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import GHC.IO (unsafePerformIO)
import PotatoCactus.Game.Definitions.GameObjectDefinitions (GameObjectDefinition (objectType), GameObjectType, objectDefinition)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject, id))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position))
import Prelude hiding (id)

data StaticGameObjectSet = StaticGameObjectSet
  { elements_ :: [GameObject]
  }

instance_ :: IORef StaticGameObjectSet
{-# NOINLINE instance_ #-}
instance_ = unsafePerformIO $ newIORef $ StaticGameObjectSet []

initializeStaticGameSet :: IO Int
initializeStaticGameSet = do
  let i =
        StaticGameObjectSet
          [ GameObject 1530 (Position 3088 3251 0) 3, -- door example
            GameObject 1530 (Position 3101 3258 0) 2, -- door example
            GameObject 1530 (Position 3101 3258 0) 2 -- diagonal door example
          ]
  writeIORef instance_ i

  return (length . elements_ $ i)

staticObjectAt :: Position -> GameObjectType -> Maybe GameObject
staticObjectAt pos objType =
  unsafePerformIO
    ( do
        set <- readIORef instance_
        return
          ( find
              ( \e ->
                  getPosition e == pos && objTypeForObject_ e == objType
              )
              (elements_ set)
          )
    )

objTypeForObject_ :: GameObject -> GameObjectType
objTypeForObject_ obj =
  case objectDefinition . id $ obj of
    Just def -> objectType def
    Nothing -> 10
