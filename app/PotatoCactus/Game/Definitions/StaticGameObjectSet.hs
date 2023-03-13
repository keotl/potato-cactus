module PotatoCactus.Game.Definitions.StaticGameObjectSet (StaticGameObjectSet, initializeStaticGameSet) where

import Data.IORef (IORef, newIORef, writeIORef)
import GHC.IO (unsafePerformIO)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Position (Position (Position))

data StaticGameObjectSet = StaticGameObjectSet
  { elements_ :: [GameObject]
  }

instance_ :: IORef (Maybe StaticGameObjectSet)
{-# NOINLINE instance_ #-}
instance_ = unsafePerformIO $ newIORef Nothing

initializeStaticGameSet :: IO ()
initializeStaticGameSet =
  writeIORef instance_ . Just $
    StaticGameObjectSet
      [ GameObject 1530 (Position 3088 3251 0) 0 -- door example
      ]

