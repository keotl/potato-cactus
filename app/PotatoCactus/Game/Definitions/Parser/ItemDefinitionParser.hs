{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Definitions.Parser.ItemDefinitionParser (parseItemDefinitionFile) where

import Data.Aeson (FromJSON, decode)
import Data.Aeson.Types (parse)
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectDefinition (GameObjectDefinition))
import PotatoCactus.Game.Definitions.Types.ItemDefinition (ItemDefinition (ItemDefinition))
import PotatoCactus.Utils.Logging (LogLevel (Error), logger)
import Prelude hiding (id)

parseItemDefinitionFile :: String -> IO [ItemDefinition]
parseItemDefinitionFile filename = do
  content <- B.readFile filename

  case decode content :: Maybe [ExtractedItemDefinition] of
    Nothing -> do
      logger_ Error $ "Parsing error on " ++ filename
      return []
    Just extracted -> return $ map assembleDefinition_ extracted

assembleDefinition_ :: ExtractedItemDefinition -> ItemDefinition
assembleDefinition_ extracted =
  ItemDefinition (id extracted) (name extracted) (stackable extracted)

data ExtractedItemDefinition = ExtractedItemDefinition
  { id :: Int,
    value :: Int,
    membersObject :: Bool,
    groundActions :: Maybe [Maybe String],
    name :: String,
    stackable :: Bool,
    description :: Maybe String,
    noteId :: Int,
    teamId :: Int,
    actions :: Maybe [Maybe String]
  }
  deriving (Show, Generic)

instance FromJSON ExtractedItemDefinition

logger_ = logger "ItemDefinitionParser"
