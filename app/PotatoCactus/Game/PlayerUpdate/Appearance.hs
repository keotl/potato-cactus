module PotatoCactus.Game.PlayerUpdate.Appearance where

data Gender = Male | Female deriving (Show)

data PlayerAppearance = PlayerAppearance
  { playerGender :: Gender,
    playerHead :: Int,
    playerBeard :: Int,
    playerChest :: Int,
    playerArms :: Int,
    playerHands :: Int,
    playerLegs :: Int,
    playerFeet :: Int,
    playerHairColour :: Int,
    playerTorsoColour :: Int,
    playerLegColour :: Int,
    playerFeetColour :: Int,
    playerSkinColour :: Int
  }

instance Show PlayerAppearance where
  show = show . playerGender

defaultPlayerAppearance :: PlayerAppearance
defaultPlayerAppearance = PlayerAppearance Male 0 10 18 26 33 36 42 0 0 0 0 0
