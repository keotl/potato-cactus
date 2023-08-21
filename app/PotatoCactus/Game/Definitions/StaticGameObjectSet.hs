module PotatoCactus.Game.Definitions.StaticGameObjectSet (StaticGameObjectSet, initializeStaticGameSet, staticObjectAt) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (find)
import Data.Maybe (fromMaybe)
import GHC.IO (unsafePerformIO)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject, id, objectType), GameObjectType, gameObjectHash, hashObject)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position))
import Prelude hiding (id)

data StaticGameObjectSet = StaticGameObjectSet
  { elements_ :: IntMap [GameObject]
  }

instance_ :: IORef StaticGameObjectSet
{-# NOINLINE instance_ #-}
instance_ = unsafePerformIO $ newIORef $ StaticGameObjectSet IntMap.empty

initializeStaticGameSet :: IO Int
initializeStaticGameSet = do
  let i =
        StaticGameObjectSet $
          IntMap.fromList $
            map
              (\obj -> (hashObject obj, [obj]))
              [ GameObject 1530 (Position 3088 3251 0) 0 3, -- door example
                GameObject 1530 (Position 3101 3258 0) 0 2, -- door example
                GameObject 1530 (Position 3092 3274 0) 9 2 -- diagonal door example
              ]
  writeIORef instance_ i

  return (length . elements_ $ i)

staticObjectAt :: Position -> GameObjectType -> [GameObject]
staticObjectAt pos objType =
  unsafePerformIO
    ( do
        collection <- readIORef instance_
        case elements_ collection IntMap.!? gameObjectHash (pos, objType) of
          Nothing -> return []
          Just x -> return x
    )
