{-# LANGUAGE RankNTypes #-}

module PotatoCactus.Interop.ScriptEngineProcess (ScriptEngineHandle, spawnScriptEngineProcess, send, recv, getInstance) where

import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString, hGetLine)
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import qualified Data.ByteString.Lazy.UTF8 as Lazy
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO (unsafePerformIO)
import PotatoCactus.Game.Scripting.Bridge.BridgeMessage (BridgeMessage)
import PotatoCactus.Game.Scripting.Bridge.ControlMessages (BridgeInitOptions (BridgeInitOptions), initializeBridgeMessage)
import PotatoCactus.Utils.Logging (LogLevel (Info), logger)
import System.IO (BufferMode (LineBuffering), Handle, hSetBuffering)
import System.Process (CreateProcess (env, std_in, std_out), ProcessHandle, StdStream (CreatePipe), createProcess, proc)

data ScriptEngineHandle = ScriptEngineHandle
  { stdin_ :: Handle,
    stdout_ :: Handle,
    process_ :: ProcessHandle
  }

instance_ :: IORef (Maybe ScriptEngineHandle)
{-# NOINLINE instance_ #-}
instance_ = unsafePerformIO $ newIORef Nothing

spawnScriptEngineProcess :: IO ScriptEngineHandle
spawnScriptEngineProcess = do
  (Just stdin, Just stdout, _, p) <- createProcess (proc "/usr/bin/python3" ["-u", "script-engine/main.py"]) {std_out = CreatePipe, std_in = CreatePipe, env = Just [("PYTHONPATH", "scripts/")]}
  let handle = ScriptEngineHandle stdin stdout p
  hSetBuffering (stdin_ handle) LineBuffering
  hSetBuffering (stdout_ handle) LineBuffering
  send handle $ initializeBridgeMessage (BridgeInitOptions 4 ["scripts/"])
  writeIORef instance_ (Just handle)
  return handle

send :: forall b. ToJSON b => ScriptEngineHandle -> BridgeMessage b -> IO ()
send handle msg = hPutStrLn (stdin_ handle) (encode msg)

recv :: ScriptEngineHandle -> IO ByteString
recv handle = hGetLine (stdout_ handle)

getInstance :: IO ScriptEngineHandle
getInstance = do
  handle <- readIORef instance_

  case handle of
    Nothing -> error "Script engine not initialized."
    Just h -> return h

logger_ = logger "ScriptEngineProcess"
