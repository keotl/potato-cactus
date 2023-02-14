module PotatoCactus.Network.Encoding.ChatMessageEncoding (decodeChatText, encodeChatText) where

import Control.Monad (replicateM)
import Data.Binary (Get, getWord8)
import Data.Binary.BitPut (BitPut, putNBits, runBitPut)
import Data.Binary.Get (runGet, skip)
import Data.Binary.Strict.BitGet (BitGet)
import Data.Bits (Bits (shiftL, xor, (.&.)), shiftR)
import Data.ByteString (ByteString, length)
import Data.ByteString.Lazy (toStrict)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import PotatoCactus.Network.Binary (toWord_)

decodeChatText :: [Word8] -> [Char]
decodeChatText [] = []
decodeChatText [x] = [mapChar_ x]
decodeChatText (x : xs)
  | x < 13 = mapChar_ x : decodeChatText xs
  | otherwise = mapChar_ (((x `shiftL` 4) + head xs) - 195) : decodeChatText (tail xs)

encodeChatText :: [Char] -> ByteString
encodeChatText input =
  toStrict $
    runBitPut
      ( mapM_ encodeText_ input
      )

encodeText_ :: Char -> BitPut
encodeText_ x
  | index < 13 = putNBits 4 $ toWord_ index
  | index > 12 = putNBits 8 $ toWord_ (195 + index)
  | otherwise = putNBits 0 $ toWord_ 0
  where
    index = charIndex_ x

charIndex_ :: Char -> Int
charIndex_ x =
  fromMaybe 0 (elemIndex x characters_)

mapChar_ :: Word8 -> Char
mapChar_ i = characters_ !! fromIntegral i

characters_ :: [Char]
characters_ =
  [ ' ',
    'e',
    't',
    'a',
    'o',
    'i',
    'h',
    'n',
    's',
    'r',
    'd',
    'l',
    'u',
    'm',
    'w',
    'c',
    'y',
    'f',
    'g',
    'p',
    'b',
    'v',
    'k',
    'x',
    'j',
    'q',
    'z',
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    ' ',
    '!',
    '?',
    '.',
    ',',
    ':',
    ';',
    '(',
    ')',
    '-',
    '&',
    '*',
    '\\',
    '\'',
    '@',
    '#',
    '+',
    '=',
    '\243',
    '$',
    '%',
    '"',
    '[',
    ']'
  ]
