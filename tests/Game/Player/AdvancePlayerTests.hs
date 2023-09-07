module Game.Player.AdvancePlayerTests where

import PotatoCactus.Game.Entity.Interaction.AdvanceInteractionDeps (AdvanceInteractionSelectors (AdvanceInteractionSelectors))
import qualified PotatoCactus.Game.Entity.Interaction.Interaction as Interaction
import PotatoCactus.Game.Entity.Interaction.Target (NpcInteractionType (NpcAction))
import qualified PotatoCactus.Game.Entity.Interaction.Target as InteractionTarget
import qualified PotatoCactus.Game.Movement.MovementEntity as PM
import PotatoCactus.Game.Player (Player (interaction, movement, skipUpdate_), create)
import PotatoCactus.Game.PlayerUpdate.AdvancePlayer (advancePlayer)
import PotatoCactus.Game.Position (Position (Position))
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
        )
    ]

pendingInteraction :: Interaction.Interaction
pendingInteraction = Interaction.createForTarget (InteractionTarget.NpcTarget 123 (NpcAction 1))

emptyInteraction :: Interaction.Interaction
emptyInteraction = Interaction.create

player :: Player
player = (create "the doctor" (Position 0 0 0)) {skipUpdate_ = False, interaction = pendingInteraction}

interactionDeps :: AdvanceInteractionSelectors
interactionDeps = AdvanceInteractionSelectors (const Nothing) (\_ _ -> Nothing)

movementPath :: [Position]
movementPath =
  [ Position 1 0 0,
    Position 2 0 0,
    Position 3 0 0
  ]
