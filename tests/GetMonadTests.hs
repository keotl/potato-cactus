{-# LANGUAGE OverloadedStrings #-}

module GetMonadTests where

import Data.Binary (Word8, getWord8)
import Data.Binary.Get (runGet)
import Data.Binary.Put (putWord8, runPut)
import Data.ByteString.Lazy
import Data.Char (ord)
import Test.HUnit
import Test.HUnit.Base (Test (..))

getTests :: Test
getTests =
  TestList
    [ TestCase (assertEqual "read bytes" [97, 98] (readByteString inputString)),
      TestCase (assertEqual "read bytes tuple" (97, 98) (readByteStringTuple inputString))
    ]

inputString :: ByteString
inputString = "abcd"

readByteString :: ByteString -> [Word8]
readByteString =
  runGet
    ( do
        first <- getWord8
        second <- getWord8
        return [first, second]
    )

readByteStringTuple :: ByteString -> (Word8, Word8)
readByteStringTuple =
  runGet
    ( do
        first <- getWord8
        second <- getWord8
        return (first, second)
    )
