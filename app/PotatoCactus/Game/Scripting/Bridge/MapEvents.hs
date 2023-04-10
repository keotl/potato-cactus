module PotatoCactus.Game.Scripting.Bridge.MapEvents (mapEvent) where
import PotatoCactus.Game.Scripting.ScriptUpdates (GameEvent)

mapEvent :: GameEvent -> String
mapEvent _ = "Event"
-- TODO - serialization  - keotl 2023-04-10
