module PotatoCactus.Game.Entity.Animation.Animation where

type AnimationId = Int

data AnimationPriority = High | Normal | Low deriving (Show)

data Animation = Animation
  { animationId :: AnimationId,
    delay :: Int,
    priority :: AnimationPriority
  }
  deriving (Show)

setAnimation :: Maybe Animation -> Animation -> Maybe Animation
setAnimation Nothing new = Just new
setAnimation (Just current) new =
  if isHigherPriority new current
    then Just new
    else Just current

isHigherPriority :: Animation -> Animation -> Bool
isHigherPriority new current =
  case priority new of
    High -> True
    Low -> False
    Normal -> case priority current of
      Low -> True
      _ -> False
