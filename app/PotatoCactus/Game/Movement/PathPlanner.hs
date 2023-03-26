module PotatoCactus.Game.Movement.PathPlanner (findPath, findPathNaive, CollisionMap) where

import PotatoCactus.Game.Movement.InterpolatePath (interpolatePath)
import PotatoCactus.Game.Position (Position (Position, x, y, z))

type CollisionMap = Int -- TODO - define this as a read-only view of the World object  - keotl 2023-03-20

findPath :: CollisionMap -> Position -> Position -> [Position]
findPath collisionMap start end =
  -- TODO - Do something smarter taking in consideration the collision map  - keotl 2023-03-20
  let mid = Position (x start) (y end) (z start)
   in case interpolatePath [start, mid, end] of
        [] -> []
        [_] -> []
        interpolated -> init interpolated

-- For NPC combat pathing
findPathNaive :: CollisionMap -> Position -> Position -> [Position]
findPathNaive =
  findPath -- TODO - implement  - keotl 2023-03-26
