module PotatoCactus.Client.GroundItemsUpdate.EncodeGroundItemsUpdate (encodeGroundItemsUpdate) where

import Data.ByteString (ByteString, concat, empty)
import PotatoCactus.Client.GroundItemsUpdate.GroundItemsUpdateDiff (GroundItemClientView, GroundItemDiff (Added, Removed, Retained), computeDiff, fromGroundItem)
import PotatoCactus.Game.Entity.GroundItem.GroundItem (GroundItem)
import PotatoCactus.Game.Entity.GroundItem.GroundItemCollection (findByChunkXYForPlayer)
import PotatoCactus.Game.Movement.PlayerMovement (hasChangedRegion)
import PotatoCactus.Game.Player (Player)
import qualified PotatoCactus.Game.Player as P
import PotatoCactus.Game.Position (GetPosition (getPosition), chunkX, chunkY)
import qualified PotatoCactus.Game.Position as Pos
import PotatoCactus.Game.World (World (groundItems))
import PotatoCactus.Network.Packets.Out.AddGroundItemPacket (addGroundItemPacket)
import PotatoCactus.Network.Packets.Out.RemoveGroundItemPacket (removeGroundItemPacket)
import PotatoCactus.Network.Packets.Out.SetPlacementReferencePacket (setPlacementReferencePacket)

encodeGroundItemsUpdate :: [GroundItemClientView] -> World -> Player -> ([GroundItemClientView], ByteString)
encodeGroundItemsUpdate oldGroundItems world player =
  let newItems = findItemsAround player world
   in if hasChangedRegion (P.movement player)
        then
          ( newItems,
            Data.ByteString.concat
              ( -- clearChunksAroundPlayer player : -- Assuming
                -- already cleared by game object update. We should
                -- probably handle both in the same function to get
                -- rid of this implicit dependency.
                map (encodeSingle player) (computeDiff [] newItems)
              )
          )
        else
          ( newItems,
            Data.ByteString.concat
              (map (encodeSingle player) (computeDiff oldGroundItems newItems))
          )

encodeSingle :: Player -> GroundItemDiff -> ByteString
encodeSingle p (Added groundItem) =
  Data.ByteString.concat
    [ setPlacementReferencePacket p (getPosition groundItem),
      addGroundItemPacket (getPosition groundItem) groundItem
    ]
encodeSingle p (Removed groundItem) =
  Data.ByteString.concat
    [ setPlacementReferencePacket p (getPosition groundItem),
      removeGroundItemPacket (getPosition groundItem) groundItem
    ]
encodeSingle p (Retained _) = empty

findItemsAround :: Player -> World -> [GroundItemClientView]
findItemsAround player world =
  let refPos = getPosition player
   in Prelude.concat
        [ map fromGroundItem $
            findByChunkXYForPlayer
              (groundItems world)
              (P.username player)
              (x + chunkX refPos, y + chunkY refPos, Pos.z refPos)
          | x <- [-2 .. 1],
            y <- [-2 .. 1]
        ]
