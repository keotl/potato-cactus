module PotatoCactus.Game.Movement.Pathing.TileFlagsUtils (mapRegionKey, mapChunkKey) where

import PotatoCactus.Game.Position (Position (..))

mapRegionKey :: Position -> Int
mapRegionKey pos =
  (x pos `div` 64) + ((y pos `div` 64) * 1000) + (z pos * 1000000)

mapChunkKey :: Position -> Int
mapChunkKey pos =
  (x pos `div` 8) + ((y pos `div` 8) * 10000) + (z pos * 100000000)
