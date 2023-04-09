{-# LANGUAGE LambdaCase #-}

module PotatoCactus.Game.World.MobList (MobList (mobs), create, add, remove, updateAll, updateAtIndex, findByIndex, findIndexByPredicate, findByPredicate, updateByPredicate, findAllByPredicate, iter, enumerate, removeByPredicate) where

import Data.List (find, findIndex)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import PotatoCactus.Utils.Iterable (replaceAtIndex)

data MobList a = MobList
  { mobs :: [Maybe a],
    availableIndices :: [Int],
    capacity_ :: Int
  }

instance Show a => Show (MobList a) where
  show m = show $ filter isJust (mobs m)

create :: (Show a) => Int -> MobList a
create capacity =
  MobList
    { mobs = replicate (capacity + 1) Nothing,
      availableIndices = [1 .. capacity],
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

removeByPredicate :: MobList a -> (a -> Bool) -> MobList a
removeByPredicate list shouldRemove =
  let removed = filter (\(i, mob) -> shouldRemove mob) (enumerate list)
   in foldl remove list (map fst removed)

-- enumerate ignoring empty spaces
enumerate :: MobList a -> [(Int, a)]
enumerate list =
  mapMaybe
    ( \(i, elem) -> case elem of
        Nothing -> Nothing
        Just e -> Just (i, e)
    )
    (zip [0 ..] (mobs list))

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

iter :: MobList a -> [a]
iter list =
  catMaybes (mobs list)
