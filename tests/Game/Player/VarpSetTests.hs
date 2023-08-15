{-# LANGUAGE BinaryLiterals #-}

module Game.Player.VarpSetTests where

import PotatoCactus.Game.PlayerUpdate.VarpSet (Varp (value), VarpSet, allValues, create, setVarbit, setVarp, updated)
import Test.HUnit

testVarpSet :: Test
testVarpSet =
  TestList
    [ TestCase
        ( assertEqual
            "set varbit should set bits between lsb/length on varp"
            [0b100000]
            (map value $ updated (setVarbit (varpId, 4, 2, 0b10) create))
        ),
      TestCase
        ( assertEqual
            "set varbit should ignore bits larger than lsb+length"
            [0]
            (map value $ updated (setVarbit (varpId, 4, 2, 0b1100) create))
        ),
      TestCase
        ( assertEqual
            "set varp should mark updated"
            [1234]
            (map value $ updated (setVarp (varpId, 1234) create))
        )
    ]

varpId = 1

collection :: VarpSet
collection = setVarp (varpId, 0b01010101) create
