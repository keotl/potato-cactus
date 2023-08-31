module PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObjectCollection, create, addDynamicObject, removeDynamicObject, findByChunkXY, iter) where

import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import PotatoCactus.Config.Constants (chunkBoundExponentXY_)
import PotatoCactus.Game.Entity.Object.DynamicObject (DynamicObject)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (objectType, position), GameObjectType)
import PotatoCactus.Game.Entity.Object.TileObjects (TileObjects)
import qualified PotatoCactus.Game.Entity.Object.TileObjects as Tile
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (z), chunkX, chunkY, positionHash)
import qualified PotatoCactus.Game.Position as Pos
import PotatoCactus.Utils.Flow ((|>))
import Prelude hiding (id)

type StaticObjectLookup = Position -> GameObjectType -> Maybe GameObject

data DynamicObjectCollection = DynamicObjectCollection
  { chunkMaps_ :: IntMap.IntMap (IntMap.IntMap TileObjects), -- chunk key -> pos/type key -> TileObjects
    staticObjectAt_ :: StaticObjectLookup
  }

instance Show DynamicObjectCollection where
  show = show . chunkMaps_

create :: StaticObjectLookup -> DynamicObjectCollection
create = DynamicObjectCollection IntMap.empty

addDynamicObject :: GameObject -> DynamicObjectCollection -> DynamicObjectCollection
addDynamicObject object collection =
  let updated =
        IntMap.alter
          ( Just
              . addToChunkMap_ object (staticObjectAt_ collection)
              . fromMaybe IntMap.empty
          )
          (objChunkKey_ object)
          (chunkMaps_ collection)
   in collection {chunkMaps_ = updated}

addToChunkMap_ :: GameObject -> StaticObjectLookup -> IntMap.IntMap TileObjects -> IntMap.IntMap TileObjects
addToChunkMap_ obj staticLookup =
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
              . removeFromChunkMap_ (pos, objType) (staticObjectAt_ collection)
              . fromMaybe IntMap.empty
          )
          (posChunkKey_ pos)
          (chunkMaps_ collection)
   in collection {chunkMaps_ = updated}

removeFromChunkMap_ :: (Position, GameObjectType) -> StaticObjectLookup -> IntMap.IntMap TileObjects -> IntMap.IntMap TileObjects
removeFromChunkMap_ (pos, objType) staticLookup =
  IntMap.alter
    ( Just
        . Tile.removeObject objType
        . fromMaybe (Tile.create (staticLookup pos))
    )
    (positionHash pos)

key_ :: GameObject -> Int
key_ object = positionHash (position object)

chunkKey_ :: Int -> Int -> Int -> Int
chunkKey_ cx cy z =
  cx
    + cy * 10 ^ chunkBoundExponentXY_
    + z * 10 ^ (2 * chunkBoundExponentXY_)

posChunkKey_ :: Position -> Int
posChunkKey_ pos =
  let cx = chunkX pos in let cy = chunkY pos in chunkKey_ cx cy (z pos)

objChunkKey_ :: GetPosition a => a -> Int
objChunkKey_ obj =
  posChunkKey_ (getPosition obj)

findByChunkXY :: Int -> Int -> Int -> DynamicObjectCollection -> [DynamicObject]
findByChunkXY cx cy z collection =
  let chunkMap =
        IntMap.findWithDefault
          IntMap.empty
          (chunkKey_ cx cy z)
          (chunkMaps_ collection)
   in iterateOverChunkMap_ chunkMap

iterateOverChunkMap_ :: IntMap.IntMap TileObjects -> [DynamicObject]
iterateOverChunkMap_ chunkMap =
  chunkMap
    |> IntMap.toList
    |> map snd
    |> concatMap Tile.objects

-- For serialization
iter :: DynamicObjectCollection -> [DynamicObject]
iter collection =
  let chunkMaps = map snd $ IntMap.toList (chunkMaps_ collection)
   in concatMap iterateOverChunkMap_ chunkMaps
