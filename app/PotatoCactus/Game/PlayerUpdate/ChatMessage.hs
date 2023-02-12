module PotatoCactus.Game.PlayerUpdate.ChatMessage where

data ChatMessage = ChatMessage
  { message :: String,
    color :: ChatMessageColor,
    effect :: ChatMessageEffect
  }

instance Show ChatMessage where
  show m = message m

type ChatMessageColor = Int

type ChatMessageEffect = Int
