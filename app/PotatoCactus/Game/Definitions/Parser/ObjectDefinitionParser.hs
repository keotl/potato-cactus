{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Definitions.Parser.ObjectDefinitionParser (parseObjectDefinitionFile) where

import Data.Aeson (FromJSON, decode)
import Data.Aeson.Types (parse)
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import PotatoCactus.Game.Definitions.Types.GameObjectDefinition (GameObjectDefinition (GameObjectDefinition))
import PotatoCactus.Utils.Logging (LogLevel (Error), logger)
import Prelude hiding (id)

parseObjectDefinitionFile :: String -> IO [GameObjectDefinition]
parseObjectDefinitionFile filename = do
  content <- B.readFile filename

  case decode content :: Maybe [ExtractedGameObjectDefinition] of
    Nothing -> do
      logger_ Error "Parsing error on definitions/objects.json"
      return []
    Just extracted -> return $ map assembleDefinition_ extracted

assembleDefinition_ :: ExtractedGameObjectDefinition -> GameObjectDefinition
assembleDefinition_ extracted =
  GameObjectDefinition (id extracted)

data ExtractedGameObjectDefinition = ExtractedGameObjectDefinition
  { id :: Int,
    name :: Maybe String,
    sizeX :: Int,
    sizeY :: Int,
    icon :: Int,
    configIds :: Int,
    childIds :: Maybe [Int],
    wall :: Bool,
    walkable :: Bool,
    solid :: Bool,
    face :: Int,
    varBitId :: Int,
    description :: Maybe String,
    hasActions :: Bool,
    animationId :: Int,
    actions :: Maybe [Maybe String]
  }
  deriving (Show, Generic)

instance FromJSON ExtractedGameObjectDefinition

logger_ = logger "ObjectDefinitionParser"
