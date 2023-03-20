module PotatoCactus.Boot.ServerInit where

import Data.Time (diffUTCTime, getCurrentTime)
import PotatoCactus.Boot.WorldInit (initializeWorld)
import PotatoCactus.Game.Definitions.EquipmentDefinitions (initializeEquipmentDefs)
import PotatoCactus.Game.Definitions.GameObjectDefinitions (initializeObjectDb)
import PotatoCactus.Game.Definitions.ItemDefinitions (initializeDb)
import PotatoCactus.Game.Definitions.StaticGameObjectSet (initializeStaticGameSet)
import PotatoCactus.Utils.Logging (LogLevel (Info), logger)
import PotatoCactus.Game.Definitions.NpcDefinitions (initializeNpcDb)

initializeServer :: IO ()
initializeServer = do
  startTime <- getCurrentTime

  itemDefs <- initializeDb
  logger_ Info $ "Loaded " ++ show itemDefs ++ " item definitions."

  equipmentDefs <- initializeEquipmentDefs
  logger_ Info $ "Loaded " ++ show equipmentDefs ++ " equipment definitions."

  objectDefs <- initializeObjectDb
  logger_ Info $ "Loaded " ++ show objectDefs ++ " game object definitions."

  npcDefs <- initializeNpcDb
  logger_ Info $ "Loaded " ++ show npcDefs ++ " game NPC definitions."

  staticObjects <- initializeStaticGameSet
  logger_ Info $ "Loaded " ++ show staticObjects ++ " static game objects."

  initializeWorld
  logger_ Info "Loaded world."

  endTime <- getCurrentTime
  logger_ Info $ "Server initialization completed in " ++ show (diffUTCTime endTime startTime)
  return ()

logger_ = logger "ServerInit"
