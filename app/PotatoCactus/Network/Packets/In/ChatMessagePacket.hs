module PotatoCactus.Network.Packets.In.ChatMessagePacket (playerChatMessage, nibbles, decodeChatText) where

import Control.Monad (replicateM)
import Data.Binary (Get, getWord8)
import Data.Binary.Get (runGet, skip)
import Data.Binary.Strict.BitGet (BitGet, getLeftByteString)
import Data.Bits (Bits (shiftL, (.&.)), shiftR)
import Data.ByteString (ByteString, drop, length, pack)
import Data.ByteString.Lazy (fromStrict)
import Data.Word (Word8)
import PotatoCactus.Boot.GameChannel
import PotatoCactus.Game.PlayerUpdate.ChatMessage (ChatMessage (ChatMessage))
import PotatoCactus.Network.Packets.Reader (InboundPacket (payload))

playerChatMessage :: String -> InboundPacket -> GameChannelMessage
playerChatMessage username packet =
  let [effect, colour] =
        runGet
          readPayload_
          (fromStrict $ payload packet)
   in PlayerChatMessage
        username
        ( ChatMessage
            (decodeChatText $ nibbles (Data.ByteString.drop 2 (payload packet)))
            (fromIntegral colour)
            (fromIntegral effect)
        )

decodeChatText :: [Word8] -> [Char]
decodeChatText [] = []
decodeChatText [x] = [mapChar_ x]
decodeChatText (x : xs)
  | x < 13 = mapChar_ x : decodeChatText xs
  | otherwise = mapChar_ (((x `shiftL` 4) + head xs) - 195) : decodeChatText (tail xs)

readPayload_ :: Get [Word8]
readPayload_ = do
  effect <- getWord8
  colour <- getWord8
  return [effect, colour]

-- message <- replicateM messageLength getWord8
-- let decoded = concatMap readChar_ message
--  in return (ChatMessage decoded (fromIntegral (128 - colour)) (fromIntegral (128 - effect)))

nibbles :: ByteString -> [Word8]
nibbles input =
  concat (runGet (getNibbles_ (Data.ByteString.length input)) (fromStrict input))

getNibbles_ :: Int -> Get [[Word8]]
getNibbles_ size = do
  replicateM
    size
    ( do
        a <- getWord8
        return [a `shiftR` 4, a .&. 0xF]
    )

-- size
-- ( do
--     a <- getWord8
--     return [a `shiftR` 4, a .&. 0xF]
-- )

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
