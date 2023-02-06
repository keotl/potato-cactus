{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module BinaryTest where

import Data.Binary.BitPut (putNBits, runBitPut)
import Data.Bits
import Data.ByteString as BS
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict)
import Data.Word
import PotatoCactus.Network.Binary (toIntME_, toShortLE_, toShort_, toWord_)
import Test.HUnit

unpackShort :: Word16 -> [Word16]
unpackShort x =
  [shiftR x 8, 255 .&. x]

unpackInt :: Word32 -> [Word32]
unpackInt x =
  [shiftR x 24, (shiftR x 16) .&. 255, (shiftR x 8) .&. 255, x .&. 255]

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

testByte :: Test
testByte =
  TestList
    [ TestCase (assertEqual "toWord" 0xFF (toWord_ 255)),
      TestCase (assertEqual "toWord" 0x0A (toWord_ 10)),
      TestCase (assertEqual "toWord negative" 246 (toWord_ (-10))),
      TestCase (assertEqual "toWord ADD" 138 (toWord_ (10 + 128))),
      TestCase (assertEqual "toWord ADD" (-118) (toWord_ (10 + 128)))
    ]

testIntME :: Test
testIntME =
  TestList
    [ TestCase (assertEqual "toIntME" [0xCD, 0x15, 0x07, 0x5B] (unpackInt $ toIntME_ 123456789)),
      TestCase (assertEqual "toIntME" [127, 177, 5, 57] (unpackInt $ toIntME_ 87654321))
    ]

testByteNegate :: Test
testByteNegate =
  TestList
    [ TestCase (assertEqual "putByte negate" 253 (toWord_ (- 3))),
      TestCase (assertEqual "putByte negate through bitput" 253 (BS.index (toStrict $ runBitPut $ putNBits 8 $ toWord_ (- 3)) 0))
    ]