module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

import qualified Data.Attoparsec.ByteString as AP
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.HashMap.Strict as HM

import Network.Socket
import Network.Socket.ByteString (recvFrom)


data Message = Get ByteString | Set ByteString ByteString
  deriving Show


message :: AP.Parser Message
message =
      (Get <$> (AP.string (pack "GET ") >> AP.takeWhile1 (\w -> w /= 10)))
  <|> (Set <$>
            ( (AP.string (pack "SET ")) >> (AP.takeWhile1 (\w -> w /= 10 && w /= 32)) )
        <*> ( (AP.word8 32)             >> (AP.takeWhile1 (\w -> w /= 10)) )
      )

awaitRecv :: Socket -> IO (ByteString, SockAddr)
awaitRecv sock = recvFrom sock 1232

handleRecv :: (ByteString, SockAddr) -> MVar (HM.HashMap ByteString ByteString) -> IO ()
handleRecv (raw, addr) mv = case msg of
  (Get k) -> do
    print msg
    m <- readMVar mv
    let v = HM.findWithDefault (pack "(nil)") k m
    print v
    reply v
    return ()
  (Set k v) -> do
    print msg
    m <- takeMVar mv
    let m2 = HM.insert k v m
    print m2
    putMVar mv m2
    return ()
  where
    (Right msg) = AP.parseOnly message raw
    reply _ = return ()

main :: IO ()
main = do
  let m = HM.empty
  mv <- newMVar m
  addr <- head <$> getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  sock <- socket (addrFamily addr) Datagram defaultProtocol
  bind sock (addrAddress addr)
  forever (awaitRecv sock >>= (\x -> forkIO (handleRecv x mv)))
