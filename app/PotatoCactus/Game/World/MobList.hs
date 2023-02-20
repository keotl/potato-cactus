{-# LANGUAGE LambdaCase #-}

module PotatoCactus.Game.World.MobList (MobList (mobs), create, add, remove, updateAll, updateAtIndex, findByIndex, findIndexByPredicate, findByPredicate, updateByPredicate, findAllByPredicate) where

import Data.List (find, findIndex)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import PotatoCactus.Utils.Iterable (replaceAtIndex)

data MobList a = MobList
  { mobs :: [Maybe a],
    availableIndices :: [Int],
    capacity_ :: Int
  }

-- data MobList a where
--   MobList ::
--     (Show a) =>
--     { mobs :: [Maybe a],
--       availableIndices :: [Int],
--       capacity_ :: Int
--     } ->
--     MobList a

instance Show a => Show (MobList a) where
  show m = show $ filter isJust (mobs m)

create :: (Show a) => Int -> MobList a
create capacity =
  MobList
    { mobs = replicate capacity Nothing,
      availableIndices = [0 .. (capacity - 1)],
      capacity_ = capacity
    }

findByIndex :: MobList a -> Int -> Maybe a
findByIndex list index = mobs list !! index

findIndexByPredicate :: MobList a -> (a -> Bool) -> Maybe Int
findIndexByPredicate list predicate =
  findIndex
    (maybe False predicate)
    (mobs list)

findByPredicate :: MobList a -> (a -> Bool) -> Maybe a
findByPredicate list predicate =
  fromMaybe Nothing (find (maybe False predicate) (mobs list))

findAllByPredicate :: MobList a -> (a -> Bool) -> [a]
findAllByPredicate list predicate =
  mapMaybe
    ( \case
        Nothing -> Nothing
        Just m -> if predicate m then Just m else Nothing
    )
    (mobs list)

data AddError = Full

add :: MobList a -> a -> Either (MobList a, Int) AddError
add list e =
  case availableIndices list of
    [] -> Right Full
    (x : xs) ->
      let nextIndex = head (availableIndices list)
       in Left
            ( list
                { mobs = replaceAtIndex nextIndex (Just e) (mobs list),
                  availableIndices = xs
                },
              nextIndex
            )

remove :: MobList a -> Int -> MobList a
remove list index =
  list
    { mobs = replaceAtIndex index Nothing (mobs list),
      availableIndices =
        if isNothing (mobs list !! index)
          then availableIndices list
          else index : availableIndices list
    }

updateAll :: MobList a -> (a -> a) -> MobList a
updateAll list transform =
  list
    { mobs = map (update_ transform) (mobs list)
    }

update_ :: (a -> a) -> Maybe a -> Maybe a
update_ _ Nothing = Nothing
update_ update (Just x) = Just $ update x

updateAtIndex :: MobList a -> Int -> (a -> a) -> MobList a
updateAtIndex list i transform =
  list
    { mobs = replaceAtIndex i (update_ transform (mobs list !! i)) (mobs list)
    }

updateByPredicate :: MobList a -> (a -> Bool) -> (a -> a) -> MobList a
updateByPredicate list predicate transform =
  case findIndexByPredicate list predicate of
    Nothing -> list
    Just index -> updateAtIndex list index transform
