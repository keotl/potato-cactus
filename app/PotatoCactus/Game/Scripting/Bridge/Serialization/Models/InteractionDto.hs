{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.InteractionDto (playerInteractionToDto, interactionToDto) where

import Data.Aeson (Object, ToJSON, Value (Null), object, (.=))
import Data.Aeson.KeyMap (empty)
import Data.Aeson.Types (Value (String))
import GHC.Generics (Generic)
import qualified PotatoCactus.Game.Entity.Interaction.Interaction as I
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (InProgress, Pending, PendingPathing))
import PotatoCactus.Game.Entity.Interaction.Target (InteractionTarget (None, NpcTarget, ObjectTarget), NpcInteractionType (NpcAction, NpcAttack))
import PotatoCactus.Game.Entity.Npc.Npc (Npc (definitionId))
import PotatoCactus.Game.Entity.Object.GameObjectKey (GameObjectKey (GameObjectKey, position))
import qualified PotatoCactus.Game.Message.ItemOnObjectPayload as IonO
import PotatoCactus.Game.Player (Player)
import qualified PotatoCactus.Game.Player as P
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
eventName I.Interaction {I.target = (ObjectTarget _ (Left _))} = "ObjectInteractionEvent"
eventName I.Interaction {I.target = (ObjectTarget _ (Right _))} = "ItemOnObjectInteractionEvent"
eventName I.Interaction {I.target = (NpcTarget _ NpcAttack)} = "NpcAttackInteractionEvent" -- TODO - Can this be consolidated with NpcAttackEvent?  - keotl 2023-04-27
eventName I.Interaction {I.target = (NpcTarget _ _)} = "NpcInteractionEvent"

interactionToDto :: I.Interaction -> Value
interactionToDto I.Interaction {I.target = None} =
  object
    [ "target" .= Null,
      "state" .= String "pending"
    ]
interactionToDto I.Interaction {I.target = (ObjectTarget (GameObjectKey id position) (Left actionIndex)), I.state = s} =
  object
    [ "target"
        .= object
          [ "type" .= String "object",
            "objectId" .= id,
            "position" .= Pos.toDto position,
            "actionIndex" .= actionIndex
          ],
      "state" .= mapState s
    ]
interactionToDto I.Interaction {I.target = (ObjectTarget (GameObjectKey id position) (Right payload)), I.state = s} =
  object
    [ "target"
        .= object
          [ "type" .= String "itemOnObject",
            "objectId" .= id,
            "position" .= Pos.toDto position,
            "itemId" .= IonO.itemId payload,
            "itemIndex" .= IonO.itemIndex payload,
            "interfaceId" .= IonO.interfaceId payload
          ],
      "state" .= mapState s
    ]
interactionToDto I.Interaction {I.target = (NpcTarget npcIndex NpcAttack), I.state = s} =
  object
    [ "target"
        .= object
          [ "type" .= String "npcAttack",
            "npcIndex" .= npcIndex
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

mapState :: InteractionState -> String
mapState Pending = "pending"
mapState PendingPathing = "pendingPathing"
mapState InProgress = "inProgress"
