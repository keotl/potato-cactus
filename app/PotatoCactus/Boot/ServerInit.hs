module PotatoCactus.Boot.ServerInit where

import Data.Time (diffUTCTime, getCurrentTime)
import PotatoCactus.Game.Definitions.EquipmentDefinitions (initializeEquipmentDefs)
import PotatoCactus.Game.Definitions.ItemDefinitions (initializeDb)
import PotatoCactus.Utils.Logging (LogLevel (Info), logger)

initializeServer :: IO ()
initializeServer = do
  startTime <- getCurrentTime

  itemDefs <- initializeDb
  logger_ Info $ "Loaded " ++ show itemDefs ++ " item definitions."

  equipmentDefs <- initializeEquipmentDefs
  logger_ Info $ "Loaded " ++ show equipmentDefs ++ " equipment definitions."

  endTime <- getCurrentTime
  logger_ Info $ "Server initialization completed in " ++ show (diffUTCTime endTime startTime)
  return ()

logger_ = logger "ServerInit"
