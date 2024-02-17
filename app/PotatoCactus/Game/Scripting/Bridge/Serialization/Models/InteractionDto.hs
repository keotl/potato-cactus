{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.InteractionDto (playerInteractionToDto, interactionToDto) where

import Data.Aeson (Object, ToJSON, Value (Null), object, (.=))
import Data.Aeson.KeyMap (empty)
import Data.Aeson.Types (Value (String))
import GHC.Generics (Generic)
import qualified PotatoCactus.Game.Entity.Interaction.Interaction as I
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (InProgress, Pending, PendingPathing))
import PotatoCactus.Game.Entity.Interaction.Target (GroundItemInteractionType (ItemPickup), InteractionTarget (GroundItemTarget, None, NpcTarget, ObjectTarget), NpcInteractionType (NpcAction), ObjectInteractionType (ItemOnObject, ObjectAction))
import PotatoCactus.Game.Entity.Npc.Npc (Npc (definitionId))
import qualified PotatoCactus.Game.Entity.Object.GameObject as GameObject
import PotatoCactus.Game.Player (Player)
import qualified PotatoCactus.Game.Player as P
import qualified PotatoCactus.Game.Scripting.Bridge.Serialization.Models.GameObjectDto as GameObjectDto
import qualified PotatoCactus.Game.Scripting.Bridge.Serialization.Models.PositionDto as Pos

playerInteractionToDto :: Player -> I.Interaction -> (String, Value)
playerInteractionToDto p i =
  ( eventName i,
    object
      [ "playerIndex" .= P.serverIndex p,
        "interaction" .= interactionToDto i
      ]
  )

eventName :: I.Interaction -> String
eventName I.Interaction {I.target = None} = "Noop"
eventName I.Interaction {I.target = (ObjectTarget _ (ObjectAction _))} = "ObjectInteractionEvent"
eventName I.Interaction {I.target = (ObjectTarget _ ItemOnObject {})} = "ItemOnObjectInteractionEvent"
eventName I.Interaction {I.target = (NpcTarget _ _)} = "NpcInteractionEvent"
eventName I.Interaction {I.target = (GroundItemTarget _ _ _ ItemPickup)} = "PickupItemInteractionEvent"

interactionToDto :: I.Interaction -> Value
interactionToDto I.Interaction {I.target = None} =
  object
    [ "target" .= Null,
      "state" .= String "pending"
    ]
interactionToDto I.Interaction {I.target = (ObjectTarget obj (ObjectAction actionIndex)), I.state = s} =
  object
    [ "target"
        .= object
          [ "type" .= String "objectAction",
            "object" .= GameObjectDto.gameObjectToDto obj,
            "actionIndex" .= actionIndex
          ],
      "state" .= mapState s
    ]
interactionToDto I.Interaction {I.target = (ObjectTarget obj (ItemOnObject interfaceId itemIndex itemId)), I.state = s} =
  object
    [ "target"
        .= object
          [ "type" .= String "itemOnObject",
            "object" .= GameObjectDto.gameObjectToDto obj,
            "itemId" .= itemId,
            "itemIndex" .= itemIndex,
            "interfaceId" .= interfaceId
          ],
      "state" .= mapState s
    ]
interactionToDto I.Interaction {I.target = (NpcTarget npcId (NpcAction actionIndex)), I.state = s} =
  object
    [ "target"
        .= object
          [ "type" .= String "npc",
            "npcIndex" .= npcId,
            "actionIndex" .= actionIndex
          ],
      "state" .= mapState s
    ]
interactionToDto I.Interaction {I.target = (GroundItemTarget itemId quantity pos ItemPickup), I.state = s} =
  object
    [ "target"
        .= object
          [ "type" .= String "groundItem",
            "itemId" .= itemId,
            "quantity" .= quantity,
            "position" .= Pos.toDto pos
          ],
      "state" .= mapState s
    ]

mapState :: InteractionState -> String
mapState Pending = "pending"
mapState PendingPathing = "pendingPathing"
mapState InProgress = "inProgress"
