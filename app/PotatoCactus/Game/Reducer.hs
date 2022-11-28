module PotatoCactus.Game.Reducer where
import PotatoCactus.Game.World (World)
import PotatoCactus.Boot.GameChannel (GameChannelMessage)

reduceWorld :: World -> GameChannelMessage -> World
reduceWorld state message =
  
  -- TODO - implement transitions - keotl 2022-11-28
  state
