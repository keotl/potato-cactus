module PotatoCactus.Game.Definitions.StaticGameObjectSet (StaticGameObjectSet, initializeStaticGameSet, objectAt, getStaticObjectSetInstance, allEntries) where

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

getStaticObjectSetInstance :: IO StaticGameObjectSet
getStaticObjectSetInstance =
  readIORef instance_

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

objectAt :: StaticGameObjectSet -> Position -> GameObjectType -> Maybe GameObject
objectAt collection pos objType =
  case fromMaybe [] (elements_ collection IntMap.!? gameObjectHash (pos, objType)) of
    [] -> Nothing
    x : xs -> Just x

-- TODO - Not sure whether multiple items on the same tile is
-- possible. Might be worth at least logging.  - keotl 2023-08-31
allEntries :: StaticGameObjectSet -> [GameObject]
allEntries staticObjects =
  concatMap snd (IntMap.toList . elements_ $ staticObjects)
