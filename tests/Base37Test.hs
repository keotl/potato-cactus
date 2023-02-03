module Base37Test where

import PotatoCactus.Network.Binary (decodeFromBase37, encodeToBase37)
import Test.HUnit

testEncode :: Test
testEncode =
  TestList
    [ TestCase (assertEqual "auie" 79740 (encodeToBase37 "auie")),
      TestCase (assertEqual "eiua" 266364 (encodeToBase37 "eiua")),
      TestCase (assertEqual "AbC1234" 2711495932 (encodeToBase37 "AbC1234")),
      TestCase (assertEqual "AbC12349" 100325349520 (encodeToBase37 "AbC12349"))
    ]

testDecode :: Test
testDecode =
  TestList
    [ TestCase (assertEqual "abc1234" "abc1234" (decodeFromBase37 2711495932))
    ]
