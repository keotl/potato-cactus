module PotatoCactus.Game.Player where

data Player = Player
  { username :: String
  } deriving (Show)

e = Player {username = "foo"}
