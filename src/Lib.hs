{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( server
    ) where

import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (length, head)
import Data.Text hiding (length, head, pack)
import Data.Word
import Data.ByteString
import Network.Socket hiding (send, sentTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (liftM, when)
import Control.Monad.Fix (fix)
import Control.Exception.Base
import Data.Binary

data RoomEvent = RoomNewClient { username :: ByteString }
  | RoomClientLeft { username :: ByteString }
  | RoomMessage { username :: ByteString, message :: ByteString }

data ClientFrameType = CFTHey | CFTMyNameIs | CFTSup | CFTRememberMe
  | CFTListClients | CFTMessage | CFTLogout

data ServerFrame = SFTHey Int
  | SFTSup
  | SFTNope
  | SFTUserError Word16 ByteString
  | SFTNewToken ByteString
  | SFTClients [ByteString]
  | SFTRoomEvent RoomEvent

server :: IO ()
server = do
  chan <- newChan :: IO (Chan (Int, RoomEvent))
  sock <- socket AF_INET Stream 0

  forkIO $ fix $ \loop -> do
    (_, msg) <- readChan chan
    loop

  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  acceptingLoop sock chan 0

acceptingLoop :: Socket -> Chan (Int, RoomEvent) -> Int -> IO ()
acceptingLoop s ch clientId = do
  conn <- accept s
  forkIO (serveClient conn ch clientId)
  acceptingLoop s ch (clientId + 1)

serveClient :: (Socket, SockAddr) -> Chan (Int, RoomEvent) -> Int -> IO ()
serveClient (s, _) ch clientId = do
  let broadcast event = writeChan ch (clientId, event)
  let specReceiveFrame frame = receiveFrame clientId ch s frame

  channelReader <- forkIO $ dupChan ch >>= readRoomEventChannel s clientId

  discardEx $ fix $ \loop -> do
    frame <- recv s 1
    when (length frame == 1) $ specReceiveFrame (head frame) >> loop

  killThread channelReader

receiveFrame :: Int -> Chan (Int, RoomEvent) -> Socket -> Word8 -> IO ()
receiveFrame clientId ch s frame = do
  case decodeFrameType frame of
    Just cft -> undefined
    Nothing -> sendAll s $ SFTUserError 1 (pack $ "Unexpected frame: " ++ [unsafeCoerce frame])
  return ()

decodeFrameType :: Word8 -> Maybe ClientFrameType
decodeFrameType frame = case frame of
  0x01 -> return CFTHey
  0x02 -> return CFTMyNameIs
  0x03 -> return CFTSup
  0x04 -> return CFTRememberMe
  0x05 -> return CFTListClients
  0x06 -> return CFTMessage
  0x07 -> return CFTLogout
  _    -> Nothing

readRoomEventChannel :: Socket -> Int -> Chan (Int, RoomEvent) -> IO ()
readRoomEventChannel socket clientId chan = do
  (causedByClientId, event) <- readChan chan
  when (clientId /= causedByClientId) $ sendAll socket (renderEvent event)
  readRoomEventChannel socket clientId chan

discardEx = handle (\(SomeException _) -> return ())

renderEvent :: RoomEvent -> ByteString
renderEvent = undefined

instance Binary ServerFrame where
  put (SFTUserError code message) = do
    put (0x04 :: Word8)
    put (code :: Word16)
    put (length message :: Word16)
    put message
  get = undefined

instance Binary RoomEvent where
  get = undefined
  put (RoomNewClient username) = do
    put (0x01 :: Word8)
    put (length username :: Word8)
    put username
  put (RoomClientLeft username) = do
    put (0x02 :: Word8)
    put (length username :: Word8)
    put username
  put (RoomMessage username message) = do
    put (0x03 :: Word8)
    put (length username :: Word8)
    put username
    put (length message :: Word64)
    put message
