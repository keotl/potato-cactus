module PotatoCactus.Client.LocalEntityList (LocalEntityList, LocalEntityStatus (Added, Removed, Retained), LocalEntity (LocalEntity), updateLocalEntities) where

import Data.List (find)
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import PotatoCactus.Config.Constants (entityViewingDistance)
import PotatoCactus.Game.Position (GetPosition (getPosition), Position, isWithin)
import PotatoCactus.Game.Typing (Advance (advance), Keyable (key))

data LocalEntityStatus = Added | Removed | Retained deriving (Show)

data LocalEntity a = LocalEntity a LocalEntityStatus deriving (Show)

type LocalEntityList a = [LocalEntity a]

updateLocalEntities :: (Keyable a) => LocalEntityList a -> [a] -> LocalEntityList a
updateLocalEntities localEntities worldEntities =
  -- Cleanup leftovers and mark Added as retained (removed in previous message)
  let cleaned = mapMaybe advanceLocalEntity_ localEntities
   in -- process removed for this message and refresh references for current tick
      let withRemoved = map (processRemovalAndRefresh_ worldEntities) cleaned
       in -- process added, up to 15 new players per message
          let withAdded = processAddition_ withRemoved worldEntities
           in withAdded

advanceLocalEntity_ :: LocalEntity a -> Maybe (LocalEntity a)
advanceLocalEntity_ (LocalEntity p Added) =
  Just (LocalEntity p Retained)
advanceLocalEntity_ (LocalEntity _ Removed) =
  Nothing
advanceLocalEntity_ (LocalEntity p Retained) =
  Just (LocalEntity p Retained)

processRemovalAndRefresh_ :: (Keyable a) => [a] -> LocalEntity a -> LocalEntity a
processRemovalAndRefresh_ worldEntities local =
  let (LocalEntity p status) = local
   in case newReference_ local worldEntities of
        Nothing -> LocalEntity p Removed
        Just p -> LocalEntity p status

newReference_ :: (Keyable a) => LocalEntity a -> [a] -> Maybe a
newReference_ (LocalEntity p status) = find (\x -> key x == key p)

processAddition_ :: (Keyable a) => [LocalEntity a] -> [a] -> [LocalEntity a]
processAddition_ currentLocalEntities worldEntities =
  let newEntities = filter (not . isAlreadyKnown_ currentLocalEntities) worldEntities
   in currentLocalEntities ++ take 15 (map (`LocalEntity` Added) newEntities)

isAlreadyKnown_ :: (Keyable a) => [LocalEntity a] -> a -> Bool
isAlreadyKnown_ known other =
  case find (\(LocalEntity p _) -> key p == key other) known of
    Nothing -> False
    Just _ -> True
