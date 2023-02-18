module IterableTests where

import PotatoCactus.Utils.Iterable (replaceAtIndex)
import Test.HUnit

testReplaceAt =
  TestList
    [ TestCase (assertEqual "replace at 0" [666, 0, 0, 0] (replaceAtIndex 0 666 (replicate 4 0))),
      TestCase (assertEqual "replace end" [0, 0, 0, 666] (replaceAtIndex 3 666 (replicate 4 0))),
      TestCase (assertEqual "replace middle" [0, 0, 666, 0] (replaceAtIndex 2 666 (replicate 4 0)))
    ]
