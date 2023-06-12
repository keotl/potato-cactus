module PotatoCactus.Game.Entity.GroundItem.GroundItemCollection (GroundItemCollection, create, insert, findByChunkXYForPlayer, findMatchingItem, advanceTime, remove) where

import qualified Data.IntMap as IntMap
import Data.List (delete, find, partition)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, mapMaybe, maybeToList)
import PotatoCactus.Config.Constants (groundItemGlobalDespawnDelay)
import PotatoCactus.Game.Definitions.ItemDefinitions (ItemId)
import PotatoCactus.Game.Entity.GroundItem.GroundItem (GroundItem)
import qualified PotatoCactus.Game.Entity.GroundItem.GroundItem as GroundItem
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as IntMap
import PotatoCactus.Game.ItemContainer (ItemStack (Empty))
import PotatoCactus.Game.Position (GetPosition (getPosition), Position (x, y, z), chunkX, chunkY)

type PlayerVisibilityKey = String

everyone :: PlayerVisibilityKey
everyone = "<everyone>"

data GroundItemCollection = GroundItemCollection
  { content_ :: Map.Map PlayerVisibilityKey (IntMap.IntMap [GroundItem])
  }
  deriving (Show)

create :: GroundItemCollection
create = GroundItemCollection Map.empty

insert :: GroundItemCollection -> GroundItem -> GroundItemCollection
insert collection item =
  collection
    { content_ =
        Map.alter
          (Just . insert_ item . fromMaybe IntMap.empty)
          (visibilityKey_ item)
          (content_ collection)
    }

insert_ :: GroundItem -> IntMap.IntMap [GroundItem] -> IntMap.IntMap [GroundItem]
insert_ item =
  IntMap.alter
    (Just . maybe [item] (item :))
    (key_ . getPosition $ item)

remove :: GroundItemCollection -> (ItemId, Int, Position, Maybe PlayerVisibilityKey) -> (ItemStack, GroundItemCollection)
remove collection (itemId, quantity, position, Just username) =
  let content = content_ collection
   in let (localRemoved, withLocalRemoved) =
            removeItem_
              (itemId, quantity, position)
              (fromMaybe IntMap.empty (content Map.!? username))
       in case localRemoved of
            -- Try to remove from player scope first
            Just removedItem ->
              ( removedItem,
                GroundItemCollection (Map.insert username withLocalRemoved content)
              )
            Nothing -> remove collection (itemId, quantity, position, Nothing)
remove GroundItemCollection {content_ = content} (itemId, quantity, position, Nothing) =
  let (removed, withRemoved) =
        removeItem_
          (itemId, quantity, position)
          (fromMaybe IntMap.empty (content Map.!? everyone))
   in case removed of
        Just removedStack -> (removedStack, GroundItemCollection (Map.insert everyone withRemoved content))
        Nothing -> (Empty, GroundItemCollection content)

removeItem_ :: (ItemId, Int, Position) -> IntMap.IntMap [GroundItem] -> (Maybe ItemStack, IntMap.IntMap [GroundItem])
removeItem_ (itemId, quantity, position) m =
  let chunkItems = fromMaybe [] (m IntMap.!? key_ position)
   in case find (GroundItem.matches (itemId, quantity, position)) chunkItems of
        Nothing -> (Nothing, m)
        Just removedItem ->
          ( Just . GroundItem.toItemStack $ removedItem,
            IntMap.adjust (delete removedItem) (key_ position) m
          )

findByChunkXYForPlayer :: GroundItemCollection -> String -> (Int, Int, Int) -> [GroundItem]
findByChunkXYForPlayer GroundItemCollection {content_ = content} username chunkPos =
  concatMap
    (itemsInChunkXY_ chunkPos . (content Map.!?))
    [username, everyone]

findMatchingItem :: (ItemId, Position, PlayerVisibilityKey) -> GroundItemCollection -> Maybe GroundItem
findMatchingItem (itemId, position, username) GroundItemCollection {content_ = content} =
  case findMatchingItem_ (itemId, position) (fromMaybe IntMap.empty (content Map.!? username)) of
    Just i -> Just i
    Nothing -> findMatchingItem_ (itemId, position) (fromMaybe IntMap.empty (content Map.!? everyone))

findMatchingItem_ :: (ItemId, Position) -> IntMap.IntMap [GroundItem] -> Maybe GroundItem
findMatchingItem_ (itemId, position) m =
  let chunkItems = fromMaybe [] (m IntMap.!? key_ position)
   in find
        ( \i ->
            GroundItem.itemId i == itemId
              && GroundItem.position i == position
        )
        chunkItems

itemsInChunkXY_ :: (Int, Int, Int) -> Maybe (IntMap.IntMap [GroundItem]) -> [GroundItem]
itemsInChunkXY_ _ Nothing =
  []
itemsInChunkXY_ chunkPos (Just m) =
  fromMaybe [] $ m IntMap.!? chunkKey_ chunkPos

advanceTime :: GroundItemCollection -> Int -> GroundItemCollection
advanceTime GroundItemCollection {content_ = content} time =
  GroundItemCollection (transitionExpired_ time content)

transitionExpired_ :: Int -> Map.Map PlayerVisibilityKey (IntMap.IntMap [GroundItem]) -> Map.Map PlayerVisibilityKey (IntMap.IntMap [GroundItem])
transitionExpired_ time m =
  let (expired, updated) =
        Map.mapAccumWithKey
          ( \expired k chunkMap ->
              let (newExpired, updated) = removeExpired_ time chunkMap
               in if k == everyone
                    then (expired, updated)
                    else (newExpired ++ expired, updated)
          )
          []
          m
   in foldl
        ( \m item ->
            Map.alter (Just . insert_ item . fromMaybe IntMap.empty) everyone m
        )
        updated
        (mapMaybe transitionItem_ expired)

transitionItem_ :: GroundItem -> Maybe GroundItem
transitionItem_ item =
  Just
    item
      { GroundItem.despawnTime = GroundItem.despawnTime item + groundItemGlobalDespawnDelay,
        GroundItem.player = Nothing
      }

removeExpired_ :: Int -> IntMap.IntMap [GroundItem] -> ([GroundItem], IntMap.IntMap [GroundItem])
removeExpired_ time =
  IntMap.mapAccum
    ( \expired l ->
        let (newExpired, active) = partition (GroundItem.isExpired time) l
         in (newExpired ++ expired, active)
    )
    []

key_ :: Position -> Int
key_ pos = chunkKey_ (chunkX pos, chunkY pos, z pos)

chunkKey_ :: (Int, Int, Int) -> Int
chunkKey_ (x, y, z) =
  x
    + y * 10 ^ 5
    + z * 10 ^ 10

visibilityKey_ :: GroundItem -> PlayerVisibilityKey
visibilityKey_ GroundItem.GroundItem {GroundItem.player = Nothing} =
  everyone
visibilityKey_ GroundItem.GroundItem {GroundItem.player = Just username} =
  username
