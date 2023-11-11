{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.State.Lazy (StateT, runStateT, get)
import Control.Monad.Trans.Class (lift)

import Data.Attoparsec.ByteString.Char8

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Sequence (Seq (..))
import qualified Data.Sequence as SQ

import Network.Socket
import Network.Socket.ByteString (recvFrom)


data ROp = PeekL | PeekR
data WOp = PushL ByteString | PushR ByteString | PopL | PopR
data Op = IROp ROp | IWOp WOp


class ReadE a where
  rExec :: a -> ROp -> Maybe ByteString

class WriteE a where
  wExec :: a -> WOp -> (a, Maybe ByteString)


data Struct = ISeq (Seq ByteString)
  deriving Show

instance ReadE Struct where
  rExec (ISeq Empty) PeekL = Nothing
  rExec (ISeq (x :<| _)) PeekL = Just x

  rExec (ISeq Empty) PeekR = Nothing
  rExec (ISeq (_ :|> x)) PeekR = Just x

instance WriteE Struct where
  wExec (ISeq Empty) (PushL x) = (ISeq $ SQ.singleton x, Nothing)
  wExec (ISeq xs) (PushL x) = (ISeq (x :<| xs), Nothing)

  wExec (ISeq Empty) (PushR x) = (ISeq $ SQ.singleton x, Nothing)
  wExec (ISeq xs) (PushR x) = (ISeq (xs :|> x), Nothing)

  wExec s@(ISeq Empty) PopL = (s, Nothing)
  wExec (ISeq (x :<| xs)) PopL = (ISeq xs, Just x)

  wExec s@(ISeq Empty) PopR = (s, Nothing)
  wExec (ISeq (xs :|> x)) PopR = (ISeq xs, Just x)

type Index = HashMap ByteString (MVar Struct)

data PState = PState
  { index :: MVar Index }


op :: Parser Op
op =
      ((IWOp . PushL) <$> ("PUSHL " *> takeByteString))
  <|> ((IWOp . PushR) <$> ("PUSHR " *> takeByteString))
  <|> ("POPL" *> endOfInput *> return (IWOp PopL))
  <|> ("POPR" *> endOfInput *> return (IWOp PopR))
  <|> ("PEEKL" *> endOfInput *> return (IROp PeekL))
  <|> ("PEEKR" *> endOfInput *> return (IROp PeekR))

exec :: (MVar Struct) -> Op -> IO (Maybe ByteString)
exec mv (IROp o) = do
  x <- readMVar mv
  let r = rExec x o
  return r
exec mv (IWOp o) = do
  x1 <- takeMVar mv
  let (x2, r) = wExec x1 o
  putMVar mv x2
  return r


awaitRecv :: Socket -> IO (ByteString, SockAddr)
awaitRecv sock = recvFrom sock 1232

handleRecv :: (ByteString, SockAddr) -> StateT PState IO ()
handleRecv (raw, addr) = do
  lift $ print raw
  let (Right o) = parseOnly op raw
  state <- get
  idx <- lift $ readMVar $ index state
  let (Just seqMV) = HM.lookup (pack "mySeq") idx
  r <- lift $ exec seqMV o
  lift $ print r
  return ()

listenR :: Socket -> StateT PState IO ()
listenR sock = forever ((lift $ awaitRecv sock) >>= handleRecv)

main :: IO ()
main = do
  let seq = ISeq (SQ.empty)
  seqMV <- newMVar seq
  let idx = (HM.singleton (pack "mySeq") seqMV) :: Index
  idxMV <- newMVar idx
  let state = PState { index = idxMV }
  stateMV <- newMVar state

  addr <- head <$> getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  sock <- socket (addrFamily addr) Datagram defaultProtocol
  bind sock (addrAddress addr)
  putStr "Listening on " >> (print $ addrAddress addr)
  runStateT (listenR sock) state
  return ()
