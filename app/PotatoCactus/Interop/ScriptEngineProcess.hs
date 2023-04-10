module PotatoCactus.Interop.ScriptEngineProcess (ScriptEngineHandle, spawnScriptEngineProcess, send, recv, getInstance) where

import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO (unsafePerformIO)
import PotatoCactus.Utils.Logging (LogLevel (Info), logger)
import System.IO (BufferMode (LineBuffering), Handle, hGetLine, hPutStrLn, hSetBuffering)
import System.Process (CreateProcess (std_in, std_out), ProcessHandle, StdStream (CreatePipe), createProcess, proc)

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
  (Just stdin, Just stdout, _, p) <- createProcess (proc "/usr/bin/python3" ["-u", "script-engine/main.py"]) {std_out = CreatePipe, std_in = CreatePipe}
  let handle = ScriptEngineHandle stdin stdout p
  hSetBuffering (stdin_ handle) LineBuffering
  hSetBuffering (stdout_ handle) LineBuffering
  writeIORef instance_ (Just handle)
  return handle

send :: ScriptEngineHandle -> String -> IO ()
send handle = hPutStrLn (stdin_ handle)

recv :: ScriptEngineHandle -> IO String
recv handle = hGetLine (stdout_ handle)

getInstance :: IO ScriptEngineHandle
getInstance = do
  handle <- readIORef instance_

  case handle of
    Nothing -> error "Script engine not initialized."
    Just h -> return h

logger_ = logger "ScriptEngineProcess"
