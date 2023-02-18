module MobListTests (testMobList) where

import PotatoCactus.Game.World.MobList (MobList, add, create, findByIndex, remove, updateAtIndex)
import Test.HUnit

mobList :: MobList Int
mobList = case add (create 10) 42 of
  Right _ -> create 10
  Left (updated, _) -> updated

testMobList =
  TestList
    [ TestCase
        ( assertBool
            "add"
            ( case add mobList 666 of
                Right _ -> False
                Left (updated, index) -> findByIndex updated index == Just 666
            )
        ),
      TestCase
        ( assertEqual
            "updateAtIndex"
            (42 + 1000)
            ( let updated = updateAtIndex mobList 0 (+ 1000)
               in case findByIndex updated 0 of
                    Nothing -> -1
                    Just x -> x
            )
        ),
      TestCase
        ( assertEqual
            "remove"
            Nothing
            ( findByIndex (remove mobList 0) 0
            )
        )
    ]
