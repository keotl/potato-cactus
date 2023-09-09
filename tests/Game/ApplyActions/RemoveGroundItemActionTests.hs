module Game.ApplyActions.RemoveGroundItemActionTests where

import Debug.Trace (trace)
import PotatoCactus.Game.Entity.GroundItem.GroundItem (GroundItem (GroundItem))
import qualified PotatoCactus.Game.Entity.GroundItem.GroundItemCollection as GroundItemCollection
import qualified PotatoCactus.Game.ItemContainer as ItemContainer
import qualified PotatoCactus.Game.Player as P
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Game.Scripting.Events.ApplyScriptActionResult (applyScriptResult)
import PotatoCactus.Game.Scripting.ScriptUpdates (ScriptActionResult (RemoveGroundItem))
import qualified PotatoCactus.Game.World as W
import qualified PotatoCactus.Game.World.MobList as MobList
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

removeGroundItemActionTests :: Test
removeGroundItemActionTests =
  TestList
    [ TestCase
        ( assertEqual
            "does not allow picking up an item twice"
            (Just [ItemContainer.ItemStack 617 1, ItemContainer.Empty])
            ( world
                |> flip applyScriptResult (RemoveGroundItem 617 1 pos (Just 1))
                |> flip applyScriptResult (RemoveGroundItem 617 1 pos (Just 1))
                |> W.players
                |> flip MobList.findByIndex 1
                |> fmap
                  ( \p ->
                      [ ItemContainer.atIndex 0 (P.inventory p),
                        ItemContainer.atIndex 1 (P.inventory p)
                      ]
                  )
            )
        )
    ]

pos :: Position
pos = Position 100 100 0

world :: W.World
world =
  W.defaultWorldValue
    |> flip W.addPlayer (P.create "player" pos)
    |> \w ->
      w
        { W.groundItems =
            GroundItemCollection.insert
              (W.groundItems w)
              (GroundItem 617 1 pos Nothing 100)
        }
