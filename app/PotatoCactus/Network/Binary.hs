module PotatoCactus.Network.Binary where

import Data.Binary (Word16, Word32, Word8)
import Data.Binary.Strict.Get (getWord16be, getWord32be, getWord32le, getWord8, runGet)
import Data.ByteString (pack, tail)
import Data.ByteString.Builder (Builder, word16BE)
import Data.ByteString.UTF8 as BSU
import Data.Char (ord)

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
