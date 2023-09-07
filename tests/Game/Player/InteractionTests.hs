module Game.Player.InteractionTests where

import PotatoCactus.Game.Entity.Interaction.Interaction (InteractionTargetStatus (Adjacent, Distant, Removed), LocateInteractionTarget, advanceInteraction)
import qualified PotatoCactus.Game.Entity.Interaction.Interaction as Interaction
import PotatoCactus.Game.Entity.Interaction.State (InteractionState (InProgress, PendingPathing))
import PotatoCactus.Game.Entity.Interaction.Target (InteractionTarget (NpcTarget), NpcInteractionType (NpcAction))
import Test.HUnit

advanceInteractionTests :: Test
advanceInteractionTests =
  TestList
    [ TestCase
        ( assertEqual
            "clears interaction when target has been removed"
            noInteraction
            (advanceInteraction (\_ -> Removed) pendingInteraction)
        ),
      TestCase
        ( assertEqual
            "Starts interaction if target is now adjacent"
            (pendingInteraction {Interaction.state = InProgress})
            (advanceInteraction (\_ -> Adjacent) pendingInteraction)
        ),
      TestCase
        ( assertEqual
            "Marks as PendingPathing if target is not adjacent when the interaction should have been triggered"
            (pendingInteraction {Interaction.state = PendingPathing})
            (advanceInteraction (\_ -> Distant) pendingInteraction)
        )
    ]

noInteraction :: Interaction.Interaction
noInteraction = Interaction.create

pendingInteraction :: Interaction.Interaction
pendingInteraction = Interaction.createForTarget (NpcTarget 123 (NpcAction 1))
