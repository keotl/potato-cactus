module Game.World.SelectorsTests where

import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as DynamicObjectCollection
import qualified PotatoCactus.Game.Entity.Object.GameObject as GameObject
import qualified PotatoCactus.Game.Position as Pos
import PotatoCactus.Game.World (World (World, objects), defaultWorldValue)
import PotatoCactus.Game.World.Selectors (findObjectAt)
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

worldSelectorsTest :: Test
worldSelectorsTest =
  TestList
    [ TestCase
        ( assertEqual
            "look up game objects from dynamic set first"
            (Just obj)
            (findObjectAt world pos 1)
        )
    ]

pos :: Pos.Position
pos = Pos.Position 100 100 0

obj :: GameObject.GameObject
obj = GameObject.GameObject 1 pos 10 0

world :: World
world =
  defaultWorldValue
    { objects =
        DynamicObjectCollection.create (\_ _ -> Nothing)
          |> DynamicObjectCollection.addDynamicObject obj
    }
