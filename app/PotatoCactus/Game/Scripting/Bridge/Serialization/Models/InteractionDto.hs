{-# LANGUAGE OverloadedStrings #-}

module PotatoCactus.Game.Scripting.Bridge.Serialization.Models.InteractionDto (playerInteractionToDto) where

import Data.Aeson (Object, ToJSON, Value (Null), object, (.=))
import Data.Aeson.KeyMap (empty)
import Data.Aeson.Types (Value (String))
import GHC.Generics (Generic)
import qualified PotatoCactus.Game.Entity.Interaction.Interaction as I
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (InProgress, Pending, PendingPathing))
import PotatoCactus.Game.Entity.Interaction.Target (InteractionTarget (None, NpcTarget, ObjectTarget), NpcInteractionType (NpcAction, NpcAttack))
import PotatoCactus.Game.Entity.Object.GameObjectKey (GameObjectKey (GameObjectKey, position))
import PotatoCactus.Game.Player (Player)
import qualified PotatoCactus.Game.Player as P
import qualified PotatoCactus.Game.Scripting.Bridge.Serialization.Models.PositionDto as Pos

playerInteractionToDto :: Player -> I.Interaction -> Value
playerInteractionToDto p i =
  object
    [ "playerIndex" .= P.serverIndex p,
      "interaction" .= toDto i
    ]

toDto :: I.Interaction -> Value
toDto I.Interaction {I.target = None} =
  object
    [ "target" .= Null,
      "state" .= String "Pending"
    ]
toDto I.Interaction {I.target = (ObjectTarget (GameObjectKey id position) actionIndex), I.state = s} =
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
toDto I.Interaction {I.target = (NpcTarget npcId (NpcAttack)), I.state = s} =
  object
    [ "target"
        .= object
          [ "type" .= String "npcAttack",
            "npcIndex" .= npcId
          ],
      "state" .= mapState s
    ]
toDto I.Interaction {I.target = (NpcTarget npcId (NpcAction actionIndex)), I.state = s} =
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
