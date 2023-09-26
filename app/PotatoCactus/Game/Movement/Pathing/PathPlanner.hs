module PotatoCactus.Game.Movement.Pathing.PathPlanner (findPath, findPathNaive, CollisionMap) where

import Debug.Trace (trace)
import PotatoCactus.Game.Movement.InterpolatePath (interpolatePath)
import PotatoCactus.Game.Movement.Pathing.CollisionMap (CollisionMap)
import qualified PotatoCactus.Game.Movement.Pathing.CollisionMap as CollisionMap
import PotatoCactus.Game.Position (Position (Position, x, y, z))

findPath :: CollisionMap -> Position -> Position -> [Position]
findPath collisionMap start end =
  -- TODO - Do something smarter taking in consideration the collision map  - keotl 2023-03-20
  let mid = Position (x start) (y end) (z start)
   in case interpolatePath [start, mid, end] of
        [] -> []
        [x] -> [x]
        interpolated -> init interpolated

-- For NPC combat pathing
findPathNaive :: CollisionMap -> Position -> Position -> [Position]
findPathNaive collisionMap a b =
  if z a /= z b
    then []
    else pickWhilePathingIsAllowed_ collisionMap a b

pickWhilePathingIsAllowed_ :: CollisionMap -> Position -> Position -> [Position]
pickWhilePathingIsAllowed_ collisionMap a b =
  case pickAllowedStep_ collisionMap a (trace (show (preferredStepAlongDominantDirection_ a b)) (preferredStepAlongDominantDirection_ a b)) of
    Nothing -> []
    Just step -> step : pickWhilePathingIsAllowed_ collisionMap step b

pickAllowedStep_ :: CollisionMap -> Position -> [Position] -> Maybe Position
pickAllowedStep_ _ _ [] = Nothing
pickAllowedStep_ collisionMap start (firstCandidate : otherCandidates) =
  if CollisionMap.allowsMovementBetween start firstCandidate collisionMap
    then Just firstCandidate
    else pickAllowedStep_ collisionMap start otherCandidates

preferredStepAlongDominantDirection_ :: Position -> Position -> [Position]
preferredStepAlongDominantDirection_ a b
  | dx == 0 && dy == 0 = []
  | dx == 0 = [a {y = y a + capDelta_ dy}]
  | dy == 0 = [a {x = x a + capDelta_ dx}]
  | otherwise =
    [ a {x = x a + capDelta_ dx, y = y a + capDelta_ dy},
      a {x = x a + capDelta_ dx},
      a {y = y a + capDelta_ dy}
    ]
  where
    (dx, dy) = (x b - x a, y b - y a)

capDelta_ :: Int -> Int
capDelta_ x
  | x > 1 = 1
  | x < -1 = -1
  | otherwise = x
