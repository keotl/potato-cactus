module PotatoCactus.Client.GameObjectUpdate.EncodeGameObjectUpdate (encodeGameObjectUpdate) where

import Data.ByteString (ByteString, concat, empty)
import PotatoCactus.Client.GameObjectUpdate.GameObjectUpdateDiff (GameObjectDiff (Added, Removed, Retained), computeDiff)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject, findByChunkXY)
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as Object
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (objectType), gameObjectHash)
import PotatoCactus.Game.Movement.MovementEntity (hasChangedRegion)
import PotatoCactus.Game.Player (Player (movement))
import PotatoCactus.Game.Position (GetPosition (getPosition), chunkX, chunkY)
import qualified PotatoCactus.Game.Position as Pos
import PotatoCactus.Game.World (World (objects))
import PotatoCactus.Network.Packets.Out.AddObjectPacket (addObjectPacket)
import PotatoCactus.Network.Packets.Out.ClearChunkObjectsPacket (clearChunksAroundPlayer)
import PotatoCactus.Network.Packets.Out.RemoveObjectPacket (removeObjectPacket)
import PotatoCactus.Network.Packets.Out.SetPlacementReferencePacket (setPlacementReferencePacket)
import PotatoCactus.Utils.Flow ((|>))

encodeGameObjectUpdate :: [DynamicObject] -> World -> Player -> ([DynamicObject], ByteString)
encodeGameObjectUpdate oldObjects world player =
  let newObjects = findObjectsAround player world
   in if hasChangedRegion (movement player)
        then
          ( newObjects,
            Data.ByteString.concat
              ( clearChunksAroundPlayer player :
                encodeOperations player (map (selectOperation player) (computeDiff [] newObjects))
              )
          )
        else
          ( newObjects,
            Data.ByteString.concat $
              encodeOperations player (map (selectOperation player) (computeDiff oldObjects newObjects))
          )

data OpType = RemoveObject GameObject | AddObject GameObject | Noop

selectOperation :: Player -> GameObjectDiff -> OpType
selectOperation p (Added object) =
  case object of
    Object.Added wrapped ->
      AddObject wrapped
    Object.Removed wrapped ->
      RemoveObject wrapped
selectOperation p (Removed object) =
  case object of
    Object.Added wrapped ->
      RemoveObject wrapped
    Object.Removed wrapped -> AddObject wrapped
selectOperation p (Retained _) = Noop

encodeOperations :: Player -> [OpType] -> [ByteString]
encodeOperations p operations =
  let removals = filter isRemoval operations
   in let additions = filter isAddition operations
       in (additions ++ removals)
            |> deduplicateOperations []
            |> map (encodeOp p)

-- Selects the first seen operation per (tile/objectType) combo and
-- ignores the rest. Used to ignore remove packets for replaced
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

encodeOp :: Player -> OpType -> ByteString
encodeOp p (AddObject wrapped) =
  Data.ByteString.concat
    [ setPlacementReferencePacket p (getPosition wrapped),
      addObjectPacket (getPosition wrapped) wrapped
    ]
encodeOp p (RemoveObject wrapped) =
  Data.ByteString.concat
    [ setPlacementReferencePacket p (getPosition wrapped),
      removeObjectPacket (getPosition wrapped) wrapped
    ]
encodeOp _ _ = empty

isRemoval :: OpType -> Bool
isRemoval (RemoveObject _) = True
isRemoval _ = False

isAddition :: OpType -> Bool
isAddition (AddObject _) = True
isAddition _ = False

findObjectsAround :: (GetPosition a) => a -> World -> [DynamicObject]
findObjectsAround player world =
  let refPos = getPosition player
   in Prelude.concat
        [ findByChunkXY (x + chunkX refPos) (y + chunkY refPos) (Pos.z refPos) (objects world)
          | x <- [-2 .. 1],
            y <- [-2 .. 1]
        ]
