module PotatoCactus.Game.Entity.Interaction.AdvanceInteractionDeps (AdvanceInteractionSelectors (..), locateInteractionTarget, findClosestInteractableTile) where

import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectId)
import PotatoCactus.Game.Definitions.Types.ItemDefinition (ItemId)
import PotatoCactus.Game.Entity.Interaction.Interaction (InteractionTargetStatus (Adjacent, Distant, Removed))
import PotatoCactus.Game.Entity.Interaction.Target (GroundItemInteractionType (ItemPickup), InteractionTarget (GroundItemTarget, None, NpcTarget, ObjectTarget))
import PotatoCactus.Game.Entity.Npc.Npc (Npc, NpcIndex)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject)
import qualified PotatoCactus.Game.Entity.Object.GameObject as GameObject
import PotatoCactus.Game.Position (GetPosition (getPosition), Position, isAdjacent)
import PotatoCactus.Game.Typing (IsEntityActive (isEntityActive))

data AdvanceInteractionSelectors = AdvanceInteractionSelectors
  { findNpc :: NpcIndex -> Maybe Npc,
    findObject :: Position -> GameObjectId -> Maybe GameObject
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
  AdvanceInteractionSelectors {findObject = findObject}
  actorPos
  ( ObjectTarget
      GameObject.GameObject
        { GameObject.id = objectId,
          GameObject.position = pos
        }
      _
    ) =
    case findObject pos objectId of
      Nothing -> Removed
      Just obj ->
        -- TODO - Check sizeX and sizeY  - keotl 2023-09-06
        if isAdjacent (getPosition obj) actorPos
          then Adjacent
          else Distant
locateInteractionTarget _ actorPos (GroundItemTarget itemId quantity pos _) =
  -- TODO - Validate if object has been removed  - keotl 2023-09-06
  if pos == actorPos || isAdjacent pos actorPos then Adjacent else Removed
locateInteractionTarget _ _ None = Removed

findClosestInteractableTile :: AdvanceInteractionSelectors -> Position -> InteractionTarget -> Maybe Position
findClosestInteractableTile
  AdvanceInteractionSelectors {findObject = findObject}
  actorPos
  ( ObjectTarget
      GameObject.GameObject
        { GameObject.id = objectId,
          GameObject.position = pos
        }
      _
    ) = case findObject pos objectId of
    Nothing -> Nothing
    -- TODO - actually path to an interactable tile, not the object tile  - keotl 2023-09-07
    Just obj -> Just $ getPosition obj
findClosestInteractableTile
  AdvanceInteractionSelectors {findNpc = findNpc}
  actorPos
  (NpcTarget npcId _) = case findNpc npcId of
    Nothing -> Nothing
    -- TODO - actually path to an interactable tile, not the current NPC tile  - keotl 2023-09-07
    Just npc -> Just $ getPosition npc
findClosestInteractableTile _ actorPos (GroundItemTarget _ _ pos ItemPickup) = Just pos
findClosestInteractableTile _ _ _ = Nothing

findActiveNpc_ :: AdvanceInteractionSelectors -> NpcIndex -> Maybe Npc
findActiveNpc_ AdvanceInteractionSelectors {findNpc = findNpc} npcId =
  case findNpc npcId of
    Nothing -> Nothing
    Just npc ->
      if isEntityActive npc
        then Just npc
        else Nothing
