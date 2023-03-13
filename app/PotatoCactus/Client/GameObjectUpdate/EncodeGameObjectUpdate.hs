module PotatoCactus.Client.GameObjectUpdate.EncodeGameObjectUpdate where

import Data.ByteString (ByteString, concat, empty)
import PotatoCactus.Client.GameObjectUpdate.GameObjectUpdateDiff (GameObjectDiff (Added), computeDiff)
import PotatoCactus.Game.Entity.Object.DynamicObjectCollection (DynamicObject, findByChunkXY)
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as Object
import PotatoCactus.Game.Movement.MovementEntity (hasChangedRegion)
import PotatoCactus.Game.Player (Player (movement))
import PotatoCactus.Game.Position (GetPosition (getPosition), chunkX, chunkY)
import PotatoCactus.Game.World (World (objects))
import PotatoCactus.Network.Packets.Out.AddObjectPacket (addObjectPacket)
import PotatoCactus.Network.Packets.Out.ClearChunkObjectsPacket (clearChunksAroundPlayer)
import PotatoCactus.Network.Packets.Out.SetPlacementReferencePacket (setPlacementReferencePacket)
import PotatoCactus.Network.Packets.Out.RemoveObjectPacket (removeObjectPacket)

encodeGameObjectUpdate :: [DynamicObject] -> World -> Player -> ([DynamicObject], ByteString)
encodeGameObjectUpdate oldObjects world player =
  let newObjects = findObjectsAround player world
   in if hasChangedRegion (movement player)
        then
          ( newObjects,
            Data.ByteString.concat
              ( clearChunksAroundPlayer player :
                map (encodeSingle player) (computeDiff [] newObjects)
              )
          )
        else
          ( newObjects,
            Data.ByteString.concat
              (map (encodeSingle player) (computeDiff oldObjects newObjects))
          )

encodeSingle :: Player -> GameObjectDiff -> ByteString
encodeSingle p (Added object) =
  case object of
    Object.Added wrapped ->
      Data.ByteString.concat
        [ setPlacementReferencePacket p (getPosition wrapped),
          addObjectPacket (getPosition wrapped) wrapped
        ]
    Object.Removed pos objType ->
      Data.ByteString.concat
        [ setPlacementReferencePacket p (pos),
         removeObjectPacket (pos) objType
        ]

findObjectsAround :: (GetPosition a) => a -> World -> [DynamicObject]
findObjectsAround player world =
  let refPos = getPosition player
   in Prelude.concat
        [ findByChunkXY (x + chunkX refPos) (y + chunkY refPos) (objects world)
          | x <- [-2 .. 1],
            y <- [-2 .. 1]
        ]
