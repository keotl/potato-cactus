module DecodeChatTests where

import Data.ByteString (length, pack)
import PotatoCactus.Boot.GameChannel (GameChannelMessage (PlayerChatMessage))
import PotatoCactus.Game.PlayerUpdate.ChatMessage (ChatMessage (..))
import PotatoCactus.Network.Binary (nibbles)
import PotatoCactus.Network.Encoding.ChatMessageEncoding (decodeChatText, encodeChatText)
import PotatoCactus.Network.Packets.In.ChatMessagePacket (playerChatMessage)
import PotatoCactus.Network.Packets.Reader (InboundPacket (InboundPacket))
import Test.HUnit

decodeChatTests :: Test
decodeChatTests =
  TestList
    [ TestCase
        ( assertEqual
            "decode message nibbles"
            "auie:zc"
            ( case playerChatMessage "playername" (InboundPacket 0 (pack [128, 128, 82, 93, 109, 209, 188])) of
                PlayerChatMessage playername (ChatMessage m _ _) -> m
                _ -> "error"
            )
        ),
      TestCase
        ( assertEqual
            "decode message text"
            "auie:zc"
            (decodeChatText $ nibbles [0x3c, 0x51, 0xED, 0xDD, 0xD2])
        ),
      TestCase
        ( assertEqual
            "decode message text2"
            "a:ee "
            (decodeChatText $ nibbles [0x3E, 0xD1, 0x10])
        )
    ]

testNibbles :: Test
testNibbles =
  TestList
    [ TestCase (assertEqual "split into nibbles" [0x3, 0xE, 0xD, 0x0] (nibbles $ [0x3E, 0xd0])),
      TestCase (assertEqual "bytestring length" 2 (Data.ByteString.length $ pack [0x3E, 0xd0]))
    ]

encodeChatTests :: Test
encodeChatTests =
  TestList [
  TestCase (assertEqual "encode chat text" (pack [0x3E, 0xD1, 0x10]) (encodeChatText "a:ee "))
           ]
