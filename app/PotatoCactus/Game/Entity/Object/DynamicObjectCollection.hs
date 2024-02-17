module PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObjectCollection, create, addDynamicObject, removeDynamicObject, findByChunkXY, iter, findVisibleObjectById, objectsInRegion) where

import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import PotatoCactus.Config.Constants (chunkBoundExponentXY_)
import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectId)
import PotatoCactus.Game.Entity.Object.DynamicObject (DynamicObject, VisibleObject (None))
import qualified PotatoCactus.Game.Entity.Object.DynamicObject as DynamicObject
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (objectType, position), GameObjectType)
import PotatoCactus.Game.Entity.Object.HierarchicalObjectReconciliation (reconcileObjects)
import PotatoCactus.Game.Entity.Object.TileObjects (TileObjects)
import qualified PotatoCactus.Game.Entity.Object.TileObjects as Tile
import PotatoCactus.Game.Movement.Pathing.TileFlagsUtils (mapChunkKey)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (Position, z), chunkX, chunkY, positionHash)
import qualified PotatoCactus.Game.Position as Pos
import PotatoCactus.Utils.Flow ((|>))
import Prelude hiding (id)

type RegionKey = Int

defaultRegionKeyFunction_ :: Position -> RegionKey
defaultRegionKeyFunction_ = mapChunkKey

type StaticObjectLookup = Position -> GameObjectType -> Maybe GameObject

type StaticObjectRegionLookup = RegionKey -> [GameObject]

data DynamicObjectCollection = DynamicObjectCollection
  { chunkMaps_ :: IntMap.IntMap (IntMap.IntMap TileObjects), -- chunk key -> pos/type key -> TileObjects
    regionKey_ :: Position -> RegionKey,
    staticObjectAt_ :: StaticObjectLookup,
    staticObjectsInRegion_ :: StaticObjectRegionLookup
  }

instance Show DynamicObjectCollection where
  show = show . chunkMaps_

create :: StaticObjectLookup -> StaticObjectRegionLookup -> DynamicObjectCollection
create = DynamicObjectCollection IntMap.empty defaultRegionKeyFunction_

addDynamicObject :: GameObject -> DynamicObjectCollection -> DynamicObjectCollection
addDynamicObject object collection =
  let updated =
        IntMap.alter
          ( Just
              . addToRegionMap_ object (staticObjectAt_ collection)
              . fromMaybe IntMap.empty
          )
          (regionKey_ collection . getPosition $ object)
          (chunkMaps_ collection)
   in collection {chunkMaps_ = updated}

addToRegionMap_ :: GameObject -> StaticObjectLookup -> IntMap.IntMap TileObjects -> IntMap.IntMap TileObjects
addToRegionMap_ obj staticLookup =
  IntMap.alter
    ( Just
        . Tile.addObject obj
        . fromMaybe
          (Tile.create (staticLookup (getPosition obj)))
    )
    (key_ obj)

removeDynamicObject :: (Position, GameObjectType) -> DynamicObjectCollection -> DynamicObjectCollection
removeDynamicObject (pos, objType) collection =
  let updated =
        IntMap.alter
          ( Just
              . removeFromRegionMap_ (pos, objType) (staticObjectAt_ collection)
              . fromMaybe IntMap.empty
          )
          (regionKey_ collection pos)
          (chunkMaps_ collection)
   in collection {chunkMaps_ = updated}

removeFromRegionMap_ :: (Position, GameObjectType) -> StaticObjectLookup -> IntMap.IntMap TileObjects -> IntMap.IntMap TileObjects
removeFromRegionMap_ (pos, objType) staticLookup =
  IntMap.alter
    ( Just
        . Tile.removeObject objType
        . fromMaybe (Tile.create (staticLookup pos))
    )
    (positionHash pos)

key_ :: GameObject -> Int
key_ object = positionHash (position object)

findByChunkXY :: (Int, Int, Int) -> DynamicObjectCollection -> [DynamicObject]
findByChunkXY chunkBaseRef collection =
  let chunkMap =
        IntMap.findWithDefault
          IntMap.empty
          (regionKey_ collection . chunkBaseToPos_ $ chunkBaseRef)
          (chunkMaps_ collection)
   in iterateOverChunkMap_ chunkMap

chunkBaseToPos_ :: (Int, Int, Int) -> Position
chunkBaseToPos_ (cxbase, cybase, z) =
  Position ((cxbase + 6) * 8) ((cybase + 6) * 8) z

iterateOverChunkMap_ :: IntMap.IntMap TileObjects -> [DynamicObject]
iterateOverChunkMap_ chunkMap =
  chunkMap
    |> IntMap.toList
    |> map snd
    |> concatMap Tile.objects

findVisibleObjectById :: Position -> GameObjectId -> DynamicObjectCollection -> VisibleObject
findVisibleObjectById pos objectId collection =
  let chunkMap = fromMaybe IntMap.empty (chunkMaps_ collection IntMap.!? regionKey_ collection pos)
   in case chunkMap IntMap.!? positionHash pos of
        Nothing -> None
        Just tileObjects -> Tile.findVisibleObjectById objectId tileObjects

objectsInRegion :: DynamicObjectCollection -> RegionKey -> [GameObject]
objectsInRegion collection regionKey =
  reconcileObjects
    (dynamicObjectsInRegion_ collection regionKey)
    (staticObjectsInRegion_ collection regionKey)

dynamicObjectsInRegion_ :: DynamicObjectCollection -> RegionKey -> [DynamicObject]
dynamicObjectsInRegion_ collection regionKey =
  chunkMaps_ collection IntMap.!? regionKey
    |> fromMaybe IntMap.empty
    |> IntMap.elems
    |> map Tile.objects
    |> concat

-- For serialization
iter :: DynamicObjectCollection -> [DynamicObject]
iter collection =
  let chunkMaps = map snd $ IntMap.toList (chunkMaps_ collection)
   in concatMap iterateOverChunkMap_ chunkMaps
