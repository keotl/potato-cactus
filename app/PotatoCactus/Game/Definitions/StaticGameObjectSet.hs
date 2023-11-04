{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module PotatoCactus.Game.Definitions.StaticGameObjectSet (StaticGameObjectSet (StaticGameObjectSet), initializeStaticGameSet, objectAt, getStaticObjectSetInstance, allEntries, FindStaticObjectById, findObjectById, createStaticObjectSet, objectsInRegion, addObject_) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (find, isSuffixOf)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import GHC.IO (unsafePerformIO)
import PotatoCactus.Game.Definitions.Parser.GameObjectPlacementParser (parseObjectPlacementFile)
import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectId)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject, id, objectType), GameObjectType, gameObjectHash, hashObject)
import PotatoCactus.Game.Movement.Pathing.TileFlagsUtils (mapChunkKey)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position))
import PotatoCactus.Utils.Flow ((|>))
import System.Directory (getDirectoryContents)
import Prelude hiding (id)

type RegionKey = Int

data StaticGameObjectSet = StaticGameObjectSet
  { regions_ :: IntMap (IntMap [GameObject]), -- RegionKey -> PositionKey -> [Objects]
    regionKey_ :: Position -> RegionKey
  }

instance Show StaticGameObjectSet where
  show = show . regions_

createStaticObjectSet :: StaticGameObjectSet
createStaticObjectSet = StaticGameObjectSet IntMap.empty defaultRegionKeyFunction_

defaultRegionKeyFunction_ :: Position -> Int
defaultRegionKeyFunction_ = mapChunkKey

instance_ :: IORef StaticGameObjectSet
{-# NOINLINE instance_ #-}
instance_ = unsafePerformIO $ newIORef createStaticObjectSet

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
        foldl
          (flip addObject_)
          (StaticGameObjectSet IntMap.empty defaultRegionKeyFunction_)
          (concat parsedObjects)

  writeIORef instance_ i

  return (length . concat $ parsedObjects)

addObject_ :: GameObject -> StaticGameObjectSet -> StaticGameObjectSet
addObject_ obj objectSet =
  objectSet
    { regions_ =
        IntMap.alter
          (Just . addObjectToRegion_ obj . fromMaybe IntMap.empty)
          (regionKey_ objectSet . getPosition $ obj)
          (regions_ objectSet)
    }

addObjectToRegion_ :: GameObject -> IntMap [GameObject] -> IntMap [GameObject]
addObjectToRegion_ obj =
  IntMap.alter
    (Just . (obj :) . fromMaybe [])
    (hashObject obj)

objectAt :: StaticGameObjectSet -> Position -> GameObjectType -> Maybe GameObject
objectAt collection pos objType =
  let regionMap = regions_ collection IntMap.!? regionKey_ collection pos
   in findObjectInRegion_ pos objType regionMap

objectsInRegion :: StaticGameObjectSet -> RegionKey -> [GameObject]
objectsInRegion collection regionKey =
  regions_ collection IntMap.!? regionKey
    |> fromMaybe IntMap.empty
    |> IntMap.elems
    |> concat

findObjectInRegion_ :: Position -> GameObjectType -> Maybe (IntMap [GameObject]) -> Maybe GameObject
findObjectInRegion_ _ _ Nothing = Nothing
findObjectInRegion_ pos objType (Just elements) =
  case fromMaybe [] (elements IntMap.!? gameObjectHash (pos, objType)) of
    [] -> Nothing
    x : xs -> Just x

-- TODO - Not sure whether multiple items on the same tile is
-- possible. Might be worth at least logging.  - keotl 2023-08-31

allEntries :: StaticGameObjectSet -> [GameObject]
allEntries staticObjects =
  (concat . concatMap IntMap.elems . IntMap.elems) (regions_ staticObjects)

type FindStaticObjectById = Position -> GameObjectId -> Maybe GameObject

instance Show (Position -> GameObjectId -> Maybe GameObject) where
  show _ = "<fn>"

findObjectById :: StaticGameObjectSet -> FindStaticObjectById
findObjectById staticObjects pos objId =
  -- TODO - Rework hashmap structure to allow more efficient lookups - keotl 2023-09-04
  mapMaybe (objectAt staticObjects pos) lookupOrder_
    |> filter ((== objId) . id)
    |> listToMaybe

lookupOrder_ :: [GameObjectType]
lookupOrder_ = [0, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20] -- lookup 0/10 first, since they are the most likely to have an object interaction
