module PotatoCactus.Game.Definitions.StaticGameObjectSet (StaticGameObjectSet, initializeStaticGameSet, staticObjectAt, objectAt) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (find, isSuffixOf)
import Data.Maybe (fromMaybe)
import GHC.IO (unsafePerformIO)
import PotatoCactus.Game.Definitions.Parser.GameObjectPlacementParser (parseObjectPlacementFile)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject, id, objectType), GameObjectType, gameObjectHash, hashObject)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position))
import PotatoCactus.Utils.Flow ((|>))
import System.Directory (getDirectoryContents)
import Prelude hiding (id)

data StaticGameObjectSet = StaticGameObjectSet
  { elements_ :: IntMap [GameObject]
  }

instance_ :: IORef StaticGameObjectSet
{-# NOINLINE instance_ #-}
instance_ = unsafePerformIO $ newIORef $ StaticGameObjectSet IntMap.empty

initializeStaticGameSet :: String -> String -> IO Int
initializeStaticGameSet mapDirectory objectFileSuffix = do
  mapFiles <- getDirectoryContents mapDirectory
  parsedObjects <-
    mapFiles
      |> filter (isSuffixOf objectFileSuffix)
      |> map (mapDirectory ++)
      |> mapM parseObjectPlacementFile

  let i =
        StaticGameObjectSet $
          IntMap.fromList $
            map
              (\obj -> (hashObject obj, [obj]))
              (concat parsedObjects)
  writeIORef instance_ i

  return (length . elements_ $ i)

staticObjectAt :: Position -> GameObjectType -> [GameObject]
staticObjectAt pos objType =
  unsafePerformIO
    ( do
        collection <- readIORef instance_
        return $ objectAt collection pos objType
    )

objectAt :: StaticGameObjectSet -> Position -> GameObjectType -> [GameObject]
objectAt collection pos objType =
  fromMaybe
    []
    (elements_ collection IntMap.!? gameObjectHash (pos, objType))
