{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad

import Data.ByteString (ByteString)
import Data.Husky.Struct.Types
import Data.Husky.Struct.Seq

import Network.Socket
import Network.Socket.ByteString (recvFrom, sendAllTo)

onRecv :: Socket -> IO (ByteString, SockAddr)
onRecv sock = recvFrom sock 1232

listenR :: Socket -> OpExec -> IO ()
listenR sock s = do
  (raw, addr) <- onRecv sock
  handle raw s >>= reply sock addr

handle :: ByteString -> OpExec -> IO (Maybe (Maybe ByteString))
handle raw (Mk fn x) = print raw >> fn raw x

reply :: Socket -> SockAddr -> Maybe (Maybe ByteString) -> IO ()
reply sock addr r = case r of
  Nothing -> print "(err)" >> sendAllTo sock "(err)" addr
  Just (Nothing) -> print "(nil)" >> sendAllTo sock "(nil)" addr
  Just (Just rx) -> print rx >> sendAllTo sock rx addr

main :: IO ()
main = do
  s <- ix
  addr <- head <$> getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  sock <- socket (addrFamily addr) Datagram defaultProtocol
  bind sock (addrAddress addr)
  putStr "Listening on " >> (print $ addrAddress addr)
  forever $ listenR sock s
  return ()

