module Game.Movement.Pathing.TileFlagsMapTests where

import qualified PotatoCactus.Game.Movement.Pathing.TileFlagsMap as TileFlagsMap
import PotatoCactus.Game.Position (Position (Position, x), y)
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

tileFlagsMapTests :: Test
tileFlagsMapTests =
  TestList
    [ TestCase
        ( assertEqual
            "sets a flag"
            0xAB
            ( TileFlagsMap.create
                |> TileFlagsMap.setTileFlags 0xAB pos
                |> TileFlagsMap.getTileFlags pos
            )
        ),
      TestCase
        ( assertEqual
            "sets many consecutive flags"
            (map (\p -> fromIntegral (x p + y p)) manyPositions)
            ( TileFlagsMap.create
                |> \colMap ->
                  foldl
                    ( \a p ->
                        TileFlagsMap.setTileFlags
                          (fromIntegral (x p + y p))
                          p
                          a
                    )
                    colMap
                    manyPositions
                    |> \colMap -> map (`TileFlagsMap.getTileFlags` colMap) manyPositions
            )
        )
    ]

pos :: Position
pos = Position 100 100 0

manyPositions :: [Position]
manyPositions = [Position x y 0 | x <- [1 .. 64], y <- [1 .. 64]]
