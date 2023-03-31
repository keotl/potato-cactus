module PotatoCactus.Game.PlayerUpdate.PlayerAnimationDefinitions where

import PotatoCactus.Game.Entity.Animation.Animation (Animation (Animation), AnimationPriority (High, Low, Normal))
import PotatoCactus.Game.Player (Player)

defenceAnimation :: Player -> Animation
defenceAnimation p =
  Animation 404 0 Low -- block no shield

attackAnimation :: Player -> Animation
attackAnimation p =
  Animation 422 0 Normal -- punch
