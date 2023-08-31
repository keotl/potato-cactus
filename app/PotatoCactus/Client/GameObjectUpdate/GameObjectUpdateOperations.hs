module PotatoCactus.Client.GameObjectUpdate.GameObjectUpdateOperations (selectOperations, OpType (..)) where

import Data.ByteString (ByteString, concat)
import PotatoCactus.Client.GameObjectUpdate.GameObjectUpdateDiff (GameObjectDiff (Added, Removed, Retained))
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject, objectType), gameObjectHash)
import PotatoCactus.Game.Player (Player (Player))
import PotatoCactus.Game.Position (getPosition)
import PotatoCactus.Utils.Flow ((|>))
import qualified PotatoCactus.Game.Entity.Object.DynamicObject as Object

data OpType = RemoveObject GameObject | AddObject GameObject | Noop deriving (Eq, Show)

selectOperations :: [GameObjectDiff] -> [OpType]
selectOperations diff =
  let operations = map selectOperation diff
   in let removals = filter isRemoval operations
       in let additions = filter isAddition operations
           in (additions ++ removals)
                |> deduplicateOperations []

selectOperation :: GameObjectDiff -> OpType
selectOperation (Added object) =
  case object of
    Object.Added wrapped ->
      AddObject wrapped
    Object.Removed wrapped ->
      RemoveObject wrapped
      -- TODO - add missing cases  - keotl 2023-08-31
selectOperation (Removed object) =
  case object of
    Object.Added wrapped ->
      RemoveObject wrapped
    Object.Removed wrapped -> AddObject wrapped
selectOperation (Retained _) = Noop

-- Selects the first seen operation per (tile/objectType) combination
-- and ignores the rest. Used to ignore remove packets for replaced
-- objects.
deduplicateOperations :: [Int] -> [OpType] -> [OpType]
deduplicateOperations _ [] = []
deduplicateOperations seen (op : xs) =
  let hash = seenHash_ op
   in if hash `elem` seen
        then deduplicateOperations seen xs
        else op : deduplicateOperations (hash : seen) xs

seenHash_ :: OpType -> Int
seenHash_ (AddObject wrapped) =
  gameObjectHash (getPosition wrapped, objectType wrapped)
seenHash_ (RemoveObject wrapped) =
  gameObjectHash (getPosition wrapped, objectType wrapped)
seenHash_ _ = 0 -- Should not happen

isRemoval :: OpType -> Bool
isRemoval (RemoveObject _) = True
isRemoval _ = False

isAddition :: OpType -> Bool
isAddition (AddObject _) = True
isAddition _ = False
