module BinaryTest where

import Data.Bits
import Data.ByteString.Builder
import Data.Word
import PotatoCactus.Network.Binary (toShortLE_, toShort_)
import Test.HUnit

unpackShort :: Word16 -> [Word16]
unpackShort x =
  [shiftR x 8, 255 .&. x]

testShortLE :: Test
testShortLE =
  TestList
    [ TestCase (assertEqual "shortLE" [0x00, 0x04] (unpackShort (toShortLE_ 1024)))
    ]

testShortBE :: Test
testShortBE =
  TestList
    [ TestCase (assertEqual "shortBE" [0x04, 0x00] (unpackShort (toShort_ 1024)))
    ]

-- testEncode :: Test
-- testEncode =
--   TestList
--     [ TestCase (assertEqual "auie" 79740 (encodeToBase37 "auie")),
--       TestCase (assertEqual "eiua" 266364 (encodeToBase37 "eiua")),
--       TestCase (assertEqual "AbC1234" 2711495932 (encodeToBase37 "AbC1234")),
--       TestCase (assertEqual "AbC12349" 100325349520 (encodeToBase37 "AbC12349"))
--     ]

-- testDecode :: Test
-- testDecode =
--   TestList
--     [ TestCase (assertEqual "abc1234" "abc1234" (decodeFromBase37 2711495932))
--     ]
