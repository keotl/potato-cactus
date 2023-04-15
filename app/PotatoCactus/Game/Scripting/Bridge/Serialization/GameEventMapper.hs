module PotatoCactus.Game.Scripting.Bridge.Serialization.GameEventMapper (mapEvent) where

import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToTextBuilder)
import PotatoCactus.Game.Scripting.Bridge.BridgeMessage (BridgeMessage, EmptyPayload)
import PotatoCactus.Game.Scripting.Bridge.ControlMessages (doneSendingEventsMessage)
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent)

mapEvent :: GameEvent -> BridgeMessage EmptyPayload
mapEvent _ = doneSendingEventsMessage

-- TODO - serialization  - keotl 2023-04-10
