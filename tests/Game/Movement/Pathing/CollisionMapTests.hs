module Game.Movement.Pathing.CollisionMapTests where

import qualified PotatoCactus.Game.Movement.Pathing.CollisionMap as CollisionMap
import PotatoCactus.Game.Position (Position (Position, x), y)
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

collisionMapTests :: Test
collisionMapTests =
  TestList
    [ TestCase
        ( assertEqual
            "sets a flag"
            0xAB
            ( CollisionMap.create
                |> CollisionMap.setTileFlags 0xAB pos
                |> CollisionMap.getTileFlags pos
            )
        ),
      TestCase
        ( assertEqual
            "sets many consecutive flags"
            (map (\p -> fromIntegral (x p + y p)) manyPositions)
            ( CollisionMap.create
                |> \colMap ->
                  foldl
                    ( \a p ->
                        CollisionMap.setTileFlags
                          (fromIntegral (x p + y p))
                          p
                          a
                    )
                    colMap
                    manyPositions
                    |> \colMap -> map (`CollisionMap.getTileFlags` colMap) manyPositions
            )
        )
    ]

pos :: Position
pos = Position 100 100 0

manyPositions :: [Position]
manyPositions = [Position x y 0 | x <- [1 .. 64], y <- [1 .. 64]]
