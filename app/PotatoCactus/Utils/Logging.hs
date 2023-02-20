module PotatoCactus.Utils.Logging where

import Control.Monad (when)
import Data.IORef (newIORef)
import Data.List (isPrefixOf)
import Data.Time
import GHC.IO (unsafePerformIO)
import GHC.IORef (readIORef)

data LogLevel = Debug | Info | Warning | Error | Fatal

logger :: String -> LogLevel -> String -> IO ()

data LoggerConfigRule = LoggerConfigRule
  { prefix :: String,
    level :: LogLevel
  }

data LoggerConfig = LoggerConfig
  { rules :: [LoggerConfigRule],
    consumer :: String -> String -> IO ()
  }

stdoutLogger :: String -> String -> IO ()
stdoutLogger prefix text = do
  timestamp <- getZonedTime
  putStrLn $ formatTime defaultTimeLocale "%F %T" timestamp ++ " [" ++ prefix ++ "] " ++ text

loggerConfig_ = unsafePerformIO $ newIORef $ LoggerConfig [LoggerConfigRule "" Info] stdoutLogger
{-# NOINLINE loggerConfig_ #-}

logger prefix l text = do
  config <- readIORef loggerConfig_
  let rule = findMostSpecificRule_ (rules config) prefix
  when (shouldLog_ (level rule) l) (consumer config prefix text)

findMostSpecificRule_ :: [LoggerConfigRule] -> String -> LoggerConfigRule
findMostSpecificRule_ rules p =
  foldl1
    ( \acc e ->
        if (prefix e `isPrefixOf` p) && length (prefix acc) >= length (prefix e)
          then e
          else acc
    )
    rules

shouldLog_ :: LogLevel -> LogLevel -> Bool
shouldLog_ Info Debug = False
shouldLog_ Warning Info = False
shouldLog_ Warning Debug = False
shouldLog_ Error Warning = False
shouldLog_ Error Info = False
shouldLog_ Error Debug = False
shouldLog_ Fatal Error = False
shouldLog_ Fatal Warning = False
shouldLog_ Fatal Info = False
shouldLog_ Fatal Debug = False
shouldLog_ _ _ = True
