module Game.Player.AdvancePlayerTests where

import PotatoCactus.Game.Entity.Interaction.AdvanceInteractionDeps (AdvanceInteractionSelectors (AdvanceInteractionSelectors))
import qualified PotatoCactus.Game.Entity.Interaction.Interaction as Interaction
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (InProgress))
import PotatoCactus.Game.Entity.Interaction.Target (NpcInteractionType (NpcAction), ObjectInteractionType (ItemOnObject, ObjectAction))
import qualified PotatoCactus.Game.Entity.Interaction.Target as InteractionTarget
import qualified PotatoCactus.Game.Entity.Object.GameObject as GameObject
import qualified PotatoCactus.Game.ItemContainer as ItemContainer
import PotatoCactus.Game.Message.ItemOnObjectPayload (ItemOnObjectPayload (ItemOnObjectPayload))
import qualified PotatoCactus.Game.Movement.MovementEntity as PM
import PotatoCactus.Game.Player (Player (droppedItemIndices, interaction, movement, skipUpdate_), create)
import qualified PotatoCactus.Game.Player as P
import PotatoCactus.Game.PlayerUpdate.AdvancePlayer (advancePlayer)
import PotatoCactus.Game.PlayerUpdate.PlayerUpdate (PlayerUpdate (DropItem, InteractWithObjectWithItem))
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Utils.Flow ((|>))
import Test.HUnit

advancePlayerTests :: Test
advancePlayerTests =
  TestList
    [ TestCase
        ( assertEqual
            "Advances interaction when movement has come to a halt"
            emptyInteraction
            ( interaction $
                advancePlayer interactionDeps player
            )
        ),
      TestCase
        ( assertEqual
            "does not update interaction when still moving"
            pendingInteraction
            ( interaction $
                advancePlayer
                  interactionDeps
                  ( player
                      { movement =
                          PM.immediatelyQueueMovement
                            (movement player)
                            movementPath
                      }
                  )
            )
        ),
      TestCase
        ( assertEqual
            "only allows dropping an item once"
            [0]
            ( droppedItemIndices $
                advancePlayer
                  interactionDeps
                  ( player
                      |> flip P.giveItem (ItemContainer.ItemStack 1 1)
                      |> flip P.queueUpdate (DropItem 3214 1 0)
                      |> flip P.queueUpdate (DropItem 3214 1 0)
                  )
            )
        ),
      TestCase
        ( assertEqual
            "issues item on object interaction when object exists and player has item in inventory"
            itemOnObjectInteraction
            ( interaction $
                advancePlayer
                  interactionDeps
                  ( player
                      |> flip P.giveItem (ItemContainer.ItemStack 1 1)
                      |> flip
                        P.queueUpdate
                        ( InteractWithObjectWithItem
                            (ItemOnObjectPayload 3214 obj 0 1)
                        )
                  )
            )
        ),
      TestCase
        ( assertEqual
            "blocks item on object interaction when player does not have the item in their inventory"
            emptyInteraction
            ( interaction $
                advancePlayer
                  interactionDeps
                  ( player
                      |> flip
                        P.queueUpdate
                        ( InteractWithObjectWithItem
                            (ItemOnObjectPayload 3214 obj 0 1)
                        )
                  )
            )
        )
    ]

pendingInteraction :: Interaction.Interaction
pendingInteraction = Interaction.createForTarget (InteractionTarget.NpcTarget 123 (NpcAction 1))

itemOnObjectInteraction :: Interaction.Interaction
itemOnObjectInteraction = (Interaction.createForTarget (InteractionTarget.ObjectTarget obj (ItemOnObject 3214 0 1))) {Interaction.state = InProgress}

emptyInteraction :: Interaction.Interaction
emptyInteraction = Interaction.create

player :: Player
player = (create "the doctor" (Position 0 0 0)) {skipUpdate_ = False, interaction = pendingInteraction}

interactionDeps :: AdvanceInteractionSelectors
interactionDeps = AdvanceInteractionSelectors (const Nothing) (\_ _ -> Just obj) (\_ -> Nothing)

movementPath :: [Position]
movementPath =
  [ Position 1 0 0,
    Position 2 0 0,
    Position 3 0 0
  ]

obj :: GameObject.GameObject
obj = GameObject.GameObject 123 (Position 1 1 0) 0 0
