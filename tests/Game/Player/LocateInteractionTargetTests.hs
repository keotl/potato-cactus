module Game.Player.LocateInteractionTargetTests where

import qualified PotatoCactus.Game.Combat.CombatEntity as Combat
import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectDefinition (GameObjectDefinition))
import qualified PotatoCactus.Game.Definitions.Types.GameObjectDefinition as ObjDef
import PotatoCactus.Game.Entity.Interaction.AdvanceInteractionDeps (AdvanceInteractionSelectors (AdvanceInteractionSelectors, findNpc, findObject, getObjectDefinition), locateInteractionTarget)
import PotatoCactus.Game.Entity.Interaction.Interaction (InteractionTargetStatus (Adjacent, Distant, Removed))
import PotatoCactus.Game.Entity.Interaction.Target (InteractionTarget (NpcTarget, ObjectTarget), NpcInteractionType (NpcAction), ObjectInteractionType (ObjectAction))
import PotatoCactus.Game.Entity.Npc.Npc (Npc, create)
import qualified PotatoCactus.Game.Entity.Npc.Npc as NPC
import PotatoCactus.Game.Entity.Npc.RespawnStrategy (respawning)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Position (Position (Position, x))
import Test.HUnit

locateInteractionTargetTests :: Test
locateInteractionTargetTests =
  TestList
    [ TestCase
        ( assertEqual
            "locates an adjacent NPC"
            Adjacent
            (locateInteractionTarget deps adjacentPos (NpcTarget 1 (NpcAction 0)))
        ),
      TestCase
        ( assertEqual
            "locates a distant NPC"
            Distant
            (locateInteractionTarget deps distantPos (NpcTarget 1 (NpcAction 0)))
        ),
      TestCase
        ( assertEqual
            "marks a dead NPC as Removed"
            Removed
            (locateInteractionTarget (deps {findNpc = const $ Just deadNpc}) adjacentPos (NpcTarget 1 (NpcAction 0)))
        ),
      TestCase
        ( assertEqual
            "locates an adjacent object"
            Adjacent
            (locateInteractionTarget deps adjacentPos (ObjectTarget object (ObjectAction 1)))
        ),
      TestCase
        ( assertEqual
            "locates an adjacent object with a large size"
            Adjacent
            ( locateInteractionTarget
                deps {getObjectDefinition = \_ -> Just largeObjDef}
                targetPos {x = 102}
                (ObjectTarget object (ObjectAction 1))
            )
        ),
      TestCase
        ( assertEqual
            "allows an object interaction if the actor is inside the object"
            Adjacent
            ( locateInteractionTarget
                deps {getObjectDefinition = \_ -> Just largeObjDef}
                targetPos
                (ObjectTarget object (ObjectAction 1))
            )
        ),
      TestCase
        ( assertEqual
            "locates a Removed object"
            Removed
            (locateInteractionTarget deps {findObject = \_ _ -> Nothing} adjacentPos (ObjectTarget object (ObjectAction 1)))
        )
    ]

targetPos :: Position
targetPos = Position 100 100 0

adjacentPos :: Position
adjacentPos = Position 100 101 0

distantPos :: Position
distantPos = Position 200 200 0

npc :: Npc
npc = create 123 targetPos (respawning targetPos 10)

deadNpc :: Npc
deadNpc = npc {NPC.combat = (Combat.create 1) {Combat.state = Combat.Dead}}

object :: GameObject
object = GameObject 123 targetPos 10 1

objDef :: GameObjectDefinition
objDef = GameObjectDefinition 123 1 1 False False True 0

largeObjDef :: GameObjectDefinition
largeObjDef = objDef {ObjDef.sizeX = 2, ObjDef.sizeY = 2}

deps :: AdvanceInteractionSelectors
deps = AdvanceInteractionSelectors (\_ -> Just npc) (\_ _ -> Just object) (\_ -> Just objDef)
