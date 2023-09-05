module Game.World.SelectorsTests where

import qualified Data.IntMap as IntMap
import PotatoCactus.Game.Definitions.StaticGameObjectSet (StaticGameObjectSet (StaticGameObjectSet), objectAt, findObjectById)
import qualified PotatoCactus.Game.Entity.Object.DynamicObjectCollection as DynamicObjectCollection
import PotatoCactus.Game.Entity.Object.GameObject (hashObject)
import qualified PotatoCactus.Game.Entity.Object.GameObject as GameObject
import qualified PotatoCactus.Game.Position as Pos
import PotatoCactus.Game.World (World (World, objects, staticObjectLookup_), defaultWorldValue)
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
        ),
      TestCase
        ( assertEqual
            "look up game objects from the static set if not found in dynamic override set"
            (Just staticObj)
            (findObjectAt world staticObjectPos 2)
        )
    ]

pos :: Pos.Position
pos = Pos.Position 100 100 0

staticObjectPos :: Pos.Position
staticObjectPos = Pos.Position 200 200 0

obj :: GameObject.GameObject
obj = GameObject.GameObject 1 pos 10 0

staticObj :: GameObject.GameObject
staticObj = GameObject.GameObject 2 staticObjectPos 10 0

staticSet :: StaticGameObjectSet
staticSet = StaticGameObjectSet (IntMap.fromList [(hashObject staticObj, [staticObj])])

world :: World
world =
  defaultWorldValue
    { objects =
        DynamicObjectCollection.create
          ( \p _ ->
              if p == staticObjectPos
                then Just staticObj
                else Nothing
          )
          |> DynamicObjectCollection.addDynamicObject obj,
      staticObjectLookup_ = findObjectById  staticSet
    }
