module PotatoCactus.Game.Entity.Interaction.AdvanceInteractionDeps (AdvanceInteractionSelectors (..), locateInteractionTarget, findClosestInteractableTile) where

import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectDefinition, GameObjectId)
import qualified PotatoCactus.Game.Definitions.Types.GameObjectDefinition as ObjDef
import PotatoCactus.Game.Definitions.Types.ItemDefinition (ItemId)
import PotatoCactus.Game.Entity.Interaction.CanInteractWithEntity (canInteractWithEntity, isInsideEntity)
import PotatoCactus.Game.Entity.Interaction.ClosestInteractableTileCalc (selectClosestInteractableTile)
import PotatoCactus.Game.Entity.Interaction.Interaction (InteractionTargetStatus (Adjacent, Distant, Removed))
import PotatoCactus.Game.Entity.Interaction.Target (GroundItemInteractionType (ItemPickup), InteractionTarget (GroundItemTarget, None, NpcTarget, ObjectTarget))
import PotatoCactus.Game.Entity.Npc.Npc (Npc, NpcIndex)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject)
import qualified PotatoCactus.Game.Entity.Object.GameObject as GameObject
import PotatoCactus.Game.Position (GetPosition (getPosition), Position, isAdjacent)
import PotatoCactus.Game.Typing (IsEntityActive (isEntityActive))

data AdvanceInteractionSelectors = AdvanceInteractionSelectors
  { findNpc :: NpcIndex -> Maybe Npc,
    findObject :: Position -> GameObjectId -> Maybe GameObject,
    getObjectDefinition :: GameObjectId -> Maybe GameObjectDefinition
    -- findGroundItem :: Position -> ItemId -> Int ->
  }

locateInteractionTarget :: AdvanceInteractionSelectors -> Position -> InteractionTarget -> InteractionTargetStatus
locateInteractionTarget deps actorPos (NpcTarget npcId _) =
  case findActiveNpc_ deps npcId of
    Nothing -> Removed
    Just npc ->
      if isAdjacent (getPosition npc) actorPos
        then Adjacent
        else Distant
locateInteractionTarget
  deps
  actorPos
  ( ObjectTarget
      GameObject.GameObject
        { GameObject.id = objectId,
          GameObject.position = pos
        }
      _
    ) =
    case findObject deps pos objectId of
      Nothing -> Removed
      Just obj ->
        let objDimensions = getObjectSize_ deps objectId
         in if canInteractWithEntity objDimensions (getPosition obj) actorPos
              || isInsideEntity objDimensions (getPosition obj) actorPos
              then Adjacent
              else Distant
locateInteractionTarget _ actorPos (GroundItemTarget itemId quantity pos _) =
  -- TODO - Validate if object has been removed  - keotl 2023-09-06
  if pos == actorPos || isAdjacent pos actorPos then Adjacent else Removed
locateInteractionTarget _ _ None = Removed

getObjectSize_ :: AdvanceInteractionSelectors -> GameObjectId -> (Int, Int)
getObjectSize_ AdvanceInteractionSelectors {getObjectDefinition = getObjectDefinition} objId =
  case getObjectDefinition objId of
    Nothing -> (1, 1)
    Just def -> (ObjDef.sizeX def, ObjDef.sizeY def)

findClosestInteractableTile :: AdvanceInteractionSelectors -> Position -> InteractionTarget -> Maybe Position
findClosestInteractableTile _ _ (ObjectTarget GameObject.GameObject {} _) = Nothing -- GameObjects cannot move, so we should never need to recarculate a path
findClosestInteractableTile
  AdvanceInteractionSelectors {findNpc = findNpc}
  actorPos
  (NpcTarget npcId _) = case findNpc npcId of
    Nothing -> Nothing
    Just npc -> Just $ selectClosestInteractableTile (1, 1) (getPosition npc) actorPos
findClosestInteractableTile _ actorPos (GroundItemTarget _ _ pos ItemPickup) = Nothing -- Ground items cannot move either
findClosestInteractableTile _ _ _ = Nothing

findActiveNpc_ :: AdvanceInteractionSelectors -> NpcIndex -> Maybe Npc
findActiveNpc_ AdvanceInteractionSelectors {findNpc = findNpc} npcId =
  case findNpc npcId of
    Nothing -> Nothing
    Just npc ->
      if isEntityActive npc
        then Just npc
        else Nothing
