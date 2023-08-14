module Game.GroudItemCollectionTests where

import PotatoCactus.Config.Constants (groundItemGlobalDespawnDelay)
import PotatoCactus.Game.Entity.GroundItem.GroundItem (GroundItem)
import qualified PotatoCactus.Game.Entity.GroundItem.GroundItem as GroundItem
import PotatoCactus.Game.Entity.GroundItem.GroundItemCollection (GroundItemCollection)
import qualified PotatoCactus.Game.Entity.GroundItem.GroundItemCollection as GroundItemCollection
import qualified PotatoCactus.Game.ItemContainer as ItemStack
import PotatoCactus.Game.Position (Position (Position), chunkX, chunkY)
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

testGroundItemCollection =
  TestList
    [ TestCase
        ( assertEqual
            "findByChunk"
            [item]
            ( GroundItemCollection.findByChunkXYForPlayer
                collection
                "some player"
                (chunkX itemPos, chunkY itemPos, 0)
            )
        ),
      TestCase
        ( assertEqual
            "findByChunk with player visibility includes both global and scoped items"
            [scopedItem, item]
            ( GroundItemCollection.findByChunkXYForPlayer
                collection
                "the doctor"
                (chunkX itemPos, chunkY itemPos, 0)
            )
        ),
      TestCase
        ( assertEqual
            "advanceTime removes expired and transitions expired scoped items to global scope"
            [transitionedScopedItem]
            ( GroundItemCollection.advanceTime
                collection
                expiryTime
                |> \c ->
                  GroundItemCollection.findByChunkXYForPlayer
                    c
                    "some player"
                    (chunkX itemPos, chunkY itemPos, 0)
            )
        ),
      TestCase
        ( assertEqual
            "advanceTime keeps non expired"
            [item]
            ( GroundItemCollection.advanceTime
                collection
                (expiryTime - 1)
                |> \c ->
                  GroundItemCollection.findByChunkXYForPlayer
                    c
                    "some player"
                    (chunkX itemPos, chunkY itemPos, 0)
            )
        ),
      TestCase
        ( assertEqual
            "remove global item returns the ground item as an item stack"
            (GroundItem.toItemStack item, [])
            ( GroundItemCollection.remove
                collection
                (1234, 1, itemPos, Nothing)
                |> \(i, c) ->
                  ( i,
                    GroundItemCollection.findByChunkXYForPlayer
                      c
                      "some player"
                      (chunkX itemPos, chunkY itemPos, 0)
                  )
            )
        ),
      TestCase
        ( assertEqual
            "remove matching local items before global items"
            (GroundItem.toItemStack scopedItem, [item])
            ( GroundItemCollection.remove
                collection
                (123, 1, itemPos, Just "the doctor")
                |> \(i, c) ->
                  ( i,
                    GroundItemCollection.findByChunkXYForPlayer
                      c
                      "the doctor"
                      (chunkX itemPos, chunkY itemPos, 0)
                  )
            )
        ),
      TestCase
        ( assertEqual
            "remove ignores out of scope items"
            (ItemStack.Empty, [scopedItem, item])
            ( GroundItemCollection.remove
                collection
                (123, 1, itemPos, Just "some player")
                |> \(i, c) ->
                  ( i,
                    GroundItemCollection.findByChunkXYForPlayer
                      c
                      "the doctor"
                      (chunkX itemPos, chunkY itemPos, 0)
                  )
            )
        ),
      TestCase
        ( assertEqual
            "findMatchingItem finds a stack matching the user command"
            (Just item)
            ( GroundItemCollection.findMatchingItem
                (1234, itemPos, "some player")
                collection
            )
        ),
      TestCase
        ( assertEqual
            "findMatchingItem finds a local item"
            (Just scopedItem)
            ( GroundItemCollection.findMatchingItem
                (123, itemPos, "the doctor")
                collection
            )
        ),
      TestCase
        ( assertEqual
            "findMatchingItem does not find an item only visible to another player"
            Nothing
            ( GroundItemCollection.findMatchingItem
                (123, itemPos, "the master")
                collection
            )
        )
    ]

itemPos = Position 100 100 0

collection :: GroundItemCollection
collection =
  foldl
    GroundItemCollection.insert
    GroundItemCollection.create
    [item, scopedItem]

expiryTime :: Int
expiryTime = 100

item :: GroundItem
item =
  GroundItem.GroundItem
    { GroundItem.itemId = 1234,
      GroundItem.quantity = 1,
      GroundItem.position = itemPos,
      GroundItem.player = Nothing,
      GroundItem.despawnTime = expiryTime
    }

scopedItem :: GroundItem
scopedItem =
  GroundItem.GroundItem
    { GroundItem.itemId = 123,
      GroundItem.quantity = 1,
      GroundItem.position = itemPos,
      GroundItem.player = Just "the doctor",
      GroundItem.despawnTime = expiryTime
    }

transitionedScopedItem :: GroundItem
transitionedScopedItem =
  GroundItem.GroundItem
    { GroundItem.itemId = 123,
      GroundItem.quantity = 1,
      GroundItem.position = itemPos,
      GroundItem.player = Nothing,
      GroundItem.despawnTime = expiryTime + groundItemGlobalDespawnDelay
    }
