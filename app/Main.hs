module Main where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import PotatoCactus.Boot.GameThreadMain (gameThreadMain)
import PotatoCactus.Boot.ServerInit (initializeServer)
import PotatoCactus.Network.SocketHandler
import PotatoCactus.Utils.Logging (LogLevel (Fatal), logger)

main :: IO ()
main = do
  initializeServer

  gameThreadId <-
    forkFinally
      gameThreadMain
      ( \x -> do
          logger_ Fatal $ "Game thread exited with value '" ++ show x ++ "'"
      )
  runTCPServer Nothing "43594" socketMain

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
          void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)

logger_ = logger "Main"
