{-# LANGUAGE BlockArguments #-}

module PotatoCactus.Network.Binary where

import Data.Binary (Word16, Word32, Word64, Word8)
import Data.Binary.BitPut (BitPut, putByteString, putNBits, runBitPut)
import Data.Binary.Strict.Get (getWord16be, getWord32be, getWord32le, getWord8, runGet)
import Data.Bits (Bits (rotateL, shiftR, (.&.)))
import Data.ByteString (length, pack, tail)
import Data.ByteString.Builder (Builder, word16BE)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.UTF8 as BSU
import Data.Char (ord)
import GHC.Char (chr)

toByte :: ByteString -> Word8
toByte bytes =
  case runGet getWord8 bytes of
    (Right val, _) -> val
    (Left val, _) -> 0

toShort :: ByteString -> Word16
toShort bytes =
  case runGet getWord16be bytes of
    (Right val, _) -> val
    (Left val, _) -> 0

toInt :: ByteString -> Word32
toInt bytes =
  case runGet getWord32be bytes of
    (Right val, _) -> val
    (Left val, _) -> 0

readStr :: ByteString -> (String, ByteString)
readStr bytes =
  let (h, r) = BSU.break (\x -> 10 == ord x) bytes
   in (toString h, Data.ByteString.tail r)

putShort :: Word16 -> Builder
putShort = word16BE

encodeToBase37 :: [Char] -> Word64
encodeToBase37 [] =
  0
encodeToBase37 (x : xs) =
  fromIntegral (toBase37Char_ x * (37 ^ Prelude.length xs)) + encodeToBase37 xs

toBase37Char_ :: Char -> Int
toBase37Char_ c
  | ord c >= ord 'A' && ord c <= ord 'Z' = ord c - ord 'A' + 1
  | ord c >= ord 'a' && ord c <= ord 'z' = ord c - ord 'a' + 1
  | ord c >= ord '0' && ord c <= ord '9' = ord c - ord '0' + 27
  | otherwise = 0

decodeFromBase37 :: Word64 -> String
decodeFromBase37 0 = ""
decodeFromBase37 x =
  let c = x `mod` 37
   in decodeFromBase37 (x `div` 37) ++ [fromBase37Char_ (fromIntegral c)]

fromBase37Char_ :: Int -> Char
fromBase37Char_ c
  | c >= 1 && c <= 26 = chr $ c + ord 'a' - 1
  | c >= 27 && c <= 37 = chr $ c + ord '0' - 27
  | otherwise = 'a'

toWord_ :: Int -> Word8
toWord_ = fromIntegral

toShort_ :: Int -> Word16
toShort_ = fromIntegral

toShortLE_ :: Int -> Word16
toShortLE_ x =
  rotateL (toShort_ x) 8

-- toInt Middle-Endian
toIntME_ :: Int -> Word32
toIntME_ x =
  rotateL (fromIntegral x) 16

encodeStr :: String -> ByteString
encodeStr input =
  toStrict $
    runBitPut
      ( do
          mapM_ mapChar_ input
          putNBits 8 $ toWord_ 10
      )

encodeStrNonTerminated :: String -> ByteString
encodeStrNonTerminated input =
  toStrict $
    runBitPut
      ( do
          mapM_ mapChar_ input
      )

mapChar_ :: Char -> BitPut
mapChar_ c = putNBits 8 $ toWord_ (ord c)

nibbles :: [Word8] -> [Word8]
nibbles = Prelude.foldr (\x -> (++) [x `shiftR` 4, x .&. 0xf]) []
