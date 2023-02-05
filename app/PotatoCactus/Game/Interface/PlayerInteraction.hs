module PotatoCactus.Game.Interface.PlayerInteraction where

data PlayerInteractionDefinition = PlayerInteractionDefinition
  { index :: Int,
    name :: String,
    pinned :: Bool -- should be left-click interaction
  }

attackInteraction = PlayerInteractionDefinition {index = 1, name = "Attack", pinned = True}

challengeInteraction = PlayerInteractionDefinition {index = 1, name = "Challenge", pinned = True}

followInteraction = PlayerInteractionDefinition {index = 3, name = "Follow", pinned = False}

tradeInteraction = PlayerInteractionDefinition {index = 4, name = "Trade with", pinned = False}
