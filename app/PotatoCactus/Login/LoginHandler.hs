module PotatoCactus.Login.LoginHandler where

import Data.ByteString (pack)
import Data.ByteString.UTF8 as BSU
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import PotatoCactus.Game.Movement.MovementEntity (playerWalkMovement)
import PotatoCactus.Game.Player as P
import qualified PotatoCactus.Game.Position as Pos
import PotatoCactus.Login.Models as L
import PotatoCactus.Network.Binary (readStr, toByte, toInt, toShort)
import PotatoCactus.Persistence.PlayerRepository (retrievePlayer)
import PotatoCactus.Utils.Logging (LogLevel (Debug, Info), logger)

handleLogin :: Socket -> IO (Maybe Player)
handleLogin sock = do
  -- handshake
  name_hash <- recv sock 1
  sendAll sock $ pack (replicate 8 0) --ignored
  sendAll sock $ pack [0] --success code
  sendAll sock $ pack (replicate 8 0) -- Random session key

  -- login type
  loginType <- recv sock 1
  rsaBlocksize <- recv sock 1

  -- RSA block
  magicId <- recv sock 1 -- hardcoded 255
  protocolVersion <- recv sock 2 -- hardcoded 317
  let ver = toShort protocolVersion
  logger_ Debug $ "protocol version: " ++ show ver
  memoryVersion <- recv sock 1
  crcs <- recv sock (9 * 4)

  expectedSize <- recv sock 1
  -- putStrLn $ "expectedSize: " ++ show (toByte expectedSize)
  -- rsaBytes <- recv sock $ fromIntegral $ toByte expectedSize -- python: stream L:33
  _ <- recv sock 1 -- hardcoded 10 generate_keys_stream
  seed1 <- recv sock 4
  seed2 <- recv sock 4
  seed3 <- recv sock 4
  seed4 <- recv sock 4

  let seed = map toInt [seed1, seed2, seed3, seed4]
  logger_ Debug $ "seed: " ++ show (map fromIntegral seed)
  uidBytes <- recv sock 4
  let uid = toInt uidBytes
  credentialBytes <- recv sock $ fromIntegral (toByte expectedSize) - 21
  let (username, remaining) = readStr credentialBytes
  let (password, _) = readStr remaining
  -- putStrLn $ "Got login request with username=" ++ username ++ ", password=" ++ password

  -- TODO let game loop handle sending the response
  sendAll sock $ pack [2] -- login success
  sendAll sock $ pack [0] -- account not flagged for botting
  sendAll sock $ pack [0] -- ignored?
  logger_ Info $ username ++ " logged in."
  -- TODO check credentials
  retrievePlayer username

logger_ = logger "LoginHandler"
