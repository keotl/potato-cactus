module PotatoCactus.Game.Movement.Pathing.TileFlagsUtils (mapRegionKey) where

import PotatoCactus.Game.Position (Position (..))

mapRegionKey :: Position -> Int
mapRegionKey pos =
  (x pos `div` 64) + ((y pos `div` 64) * 1000) + (z pos * 1000000)
