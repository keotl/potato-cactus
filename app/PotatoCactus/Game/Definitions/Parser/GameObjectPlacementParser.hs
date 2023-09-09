{-# LANGUAGE DeriveGeneric #-}

module PotatoCactus.Game.Definitions.Parser.GameObjectPlacementParser where

import Data.Aeson (FromJSON, Value (Object), decode, (.:))
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (FromJSON (parseJSON))
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import PotatoCactus.Game.Entity.Object.GameObject (GameObject (GameObject))
import PotatoCactus.Game.Position (Position (Position))
import PotatoCactus.Utils.Logging (LogLevel (Error), logger)

parseObjectPlacementFile :: String -> IO [GameObject]
parseObjectPlacementFile filename = do
  content <- B.readFile filename
  case decode content :: Maybe [ExtractedGameObjectPlacement] of
    Nothing -> do
      logger_ Error $ "Parsing error on " ++ filename
      return []
    Just extracted -> return $ map assembleObject_ extracted

assembleObject_ :: ExtractedGameObjectPlacement -> GameObject
assembleObject_ extracted =
  GameObject
    (objectId extracted)
    (Position (x extracted) (y extracted) (plane extracted))
    (objType extracted)
    (orientation extracted)

data ExtractedGameObjectPlacement = ExtractedGameObjectPlacement
  { objectId :: Int,
    x :: Int,
    y :: Int,
    plane :: Int,
    objType :: Int,
    orientation :: Int
  }
  deriving (Show, Generic)

instance FromJSON ExtractedGameObjectPlacement where
  parseJSON (Object x) =
    ExtractedGameObjectPlacement
      <$> x .: fromString "objectId"
      <*> x .: fromString "x"
      <*> x .: fromString "y"
      <*> x .: fromString "plane"
      <*> x .: fromString "type"
      <*> x .: fromString "orientation"
  parseJSON _ = fail "Expected an Object"

logger_ = logger "GameObjectPlacementParser"
