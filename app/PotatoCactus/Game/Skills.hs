module PotatoCactus.Game.Skills where

import Prelude hiding (id)

data SkillDefinition = SkillDefinition
  { id :: Int,
    name :: String
  }

data Skill = Skill
  { skill :: SkillDefinition,
    experience :: Int
  }

attackSkill = SkillDefinition {id = 0, name = "Attack"}

defenceSkill = SkillDefinition {id = 1, name = "Defence"}

strengthSkill = SkillDefinition {id = 2, name = "Strength"}

constitutionSkill = SkillDefinition {id = 3, name = "Constitution"}

rangedSkill = SkillDefinition {id = 4, name = "Ranged"}

prayerSkill = SkillDefinition {id = 5, name = "Prayer"}

magicSkill = SkillDefinition {id = 6, name = "Magic"}

cookingSkill = SkillDefinition {id = 7, name = "Cooking"}

woodcuttingSkill = SkillDefinition {id = 8, name = "Woodcutting"}

fletchingSkill = SkillDefinition {id = 9, name = "Fletching"}

fishingSkill = SkillDefinition {id = 10, name = "Fishing"}

firemakingSkill = SkillDefinition {id = 11, name = "Firemaking"}

craftingSkill = SkillDefinition {id = 12, name = "Crafting"}

smithingSkill = SkillDefinition {id = 13, name = "Smithing"}

miningSkill = SkillDefinition {id = 14, name = "Mining"}

herbloreSkill = SkillDefinition {id = 15, name = "Herblore"}

agilitySkill = SkillDefinition {id = 16, name = "Agility"}

thievingSkill = SkillDefinition {id = 17, name = "Thieving"}

slayerSkill = SkillDefinition {id = 18, name = "Slayer"}

farmingSkill = SkillDefinition {id = 19, name = "Farming"}

runecraftingSkill = SkillDefinition {id = 20, name = "Runecrafting"}

allSkills =
  [ attackSkill,
    defenceSkill,
    strengthSkill,
    constitutionSkill,
    rangedSkill,
    prayerSkill,
    magicSkill,
    cookingSkill,
    woodcuttingSkill,
    fletchingSkill,
    fishingSkill,
    firemakingSkill,
    craftingSkill,
    smithingSkill,
    miningSkill,
    herbloreSkill,
    agilitySkill,
    thievingSkill,
    slayerSkill,
    farmingSkill,
    runecraftingSkill
  ]

experienceToLevel :: Int -> Int
experienceToLevel xp =
  1 + min (xp `div` 100) 98
