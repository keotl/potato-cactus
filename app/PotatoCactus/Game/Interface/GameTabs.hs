module PotatoCactus.Game.Interface.GameTabs where

import Prelude hiding (id)

data TabDefinition = TabDefinition
  { id :: Int,
    defaultTab :: Int
  }

combatTab = TabDefinition {id = 0, defaultTab = 2423}

skillTab = TabDefinition {id = 1, defaultTab = 3917}

questTab = TabDefinition {id = 2, defaultTab = 638}

inventoryTab = TabDefinition {id = 3, defaultTab = 3213}

equipmentTab = TabDefinition {id = 4, defaultTab = 1644}

prayerTab = TabDefinition {id = 5, defaultTab = 5608}

magicTab = TabDefinition {id = 6, defaultTab = 1151}

unusedTab = TabDefinition {id = 7, defaultTab = -1}

friendsTab = TabDefinition {id = 8, defaultTab = 5065}

ignoresTab = TabDefinition {id = 9, defaultTab = 5715}

logoutTab = TabDefinition {id = 10, defaultTab = 2449}

settingsTab = TabDefinition {id = 11, defaultTab = 904}

emoteTab = TabDefinition {id = 12, defaultTab = 147}

musicTab = TabDefinition {id = 13, defaultTab = 962}

allTabs =
  [ combatTab,
    skillTab,
    questTab,
    inventoryTab,
    equipmentTab,
    prayerTab,
    magicTab,
    unusedTab,
    friendsTab,
    ignoresTab,
    logoutTab,
    settingsTab,
    emoteTab,
    musicTab
  ]
