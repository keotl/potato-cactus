module PotatoCactus.Game.Movement.InterpolatePath where

import PotatoCactus.Game.Position (Position (x), y)

interpolate :: Position -> Position -> [Position]
interpolate start end =
  let (dx, dy) = (x end - x start, y end - y start)
   in case (dx, dy) of
        (0, 0) -> []
        (dx, dy) ->
          start {x = x start + sign_ dx, y = y start + sign_ dy} :
          interpolate
            (start {x = x start + sign_ dx, y = y start + sign_ dy})
            end

sign_ :: Int -> Int
sign_ x
  | x >= 1 = 1
  | x <= -1 = -1
  | otherwise = 0

interpolatePath :: [Position] -> [Position]
interpolatePath [] = []
interpolatePath [a] = [a]
interpolatePath [a, b] = interpolate a b
interpolatePath (a : tail) = interpolate a (head tail) ++ interpolatePath tail
