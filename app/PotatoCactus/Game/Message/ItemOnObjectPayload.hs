module PotatoCactus.Game.Message.ItemOnObjectPayload where

import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectId)
import PotatoCactus.Game.Movement.PositionXY (PositionXY)
import PotatoCactus.Game.Scripting.Actions.CreateInterface (WidgetId)

data ItemOnObjectPayload = ItemOnObjectPayload
  { interfaceId :: WidgetId,
    objectId :: GameObjectId,
    position :: PositionXY,
    itemIndex :: Int,
    itemId :: Int
  }
  deriving (Show)
