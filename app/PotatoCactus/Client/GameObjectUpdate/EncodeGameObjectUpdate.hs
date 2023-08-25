module PotatoCactus.Client.GameObjectUpdate.EncodeGameObjectUpdate (encodeGameObjectUpdate) where

import Data.ByteString (ByteString, concat, empty)
import PotatoCactus.Client.GameObjectUpdate.GameObjectUpdateDiff (GameObjectDiff (Added, Removed, Retained), computeDiff)
import PotatoCactus.Client.GameObjectUpdate.GameObjectUpdateOperations (OpType (AddObject, RemoveObject), selectOperations)
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
            Data.ByteString.concat $
              clearChunksAroundPlayer player :
              ( computeDiff [] newObjects
                  |> selectOperations
                  |> map (encodeOp player)
              )
          )
        else
          ( newObjects,
            Data.ByteString.concat
              ( computeDiff oldObjects newObjects
                  |> selectOperations
                  |> map (encodeOp player)
              )
          )

findObjectsAround :: (GetPosition a) => a -> World -> [DynamicObject]
findObjectsAround player world =
  let refPos = getPosition player
   in Prelude.concat
        [ findByChunkXY (x + chunkX refPos) (y + chunkY refPos) (Pos.z refPos) (objects world)
          | x <- [-2 .. 1],
            y <- [-2 .. 1]
        ]

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
