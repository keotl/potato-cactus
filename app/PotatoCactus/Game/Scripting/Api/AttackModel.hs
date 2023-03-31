module PotatoCactus.Game.Scripting.Api.AttackModel where

import PotatoCactus.Game.Combat.Hit
import PotatoCactus.Game.Entity.Animation.Animation (AnimationId)

data AttackModel = AttackModel
  { hit :: Hit,
    animation :: AnimationId
  }
