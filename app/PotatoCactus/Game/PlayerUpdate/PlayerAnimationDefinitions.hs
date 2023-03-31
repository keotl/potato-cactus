module PotatoCactus.Game.PlayerUpdate.PlayerAnimationDefinitions where

import PotatoCactus.Game.Entity.Animation.Animation (Animation (Animation), AnimationPriority (High))
import PotatoCactus.Game.Player (Player)

defenceAnimation :: Player -> Animation
defenceAnimation p =
  Animation 404 0 High -- block no shield
