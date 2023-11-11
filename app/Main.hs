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

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Sequence (Seq (..))
import qualified Data.Sequence as SQ

import Network.Socket
import Network.Socket.ByteString (recvFrom)


data ROp =
    PeekL
  | PeekR
  | Get ByteString
  deriving Show

data WOp =
    PushL ByteString
  | PushR ByteString
  | PopL
  | PopR
  | Put ByteString ByteString
  deriving Show

data Op =
    IROp ROp
  | IWOp WOp
  deriving Show


class ReadE a where
  rExec :: a -> ROp -> Maybe ByteString

class WriteE a where
  wExec :: a -> WOp -> (a, Maybe ByteString)


data Struct =
    ISeq (Seq ByteString)
  | IHashMap (HashMap ByteString ByteString)
  deriving Show

data OnStruct a = On ByteString a

instance ReadE Struct where
  rExec (ISeq Empty) PeekL = Nothing
  rExec (ISeq (x :<| _)) PeekL = Just x

  rExec (ISeq Empty) PeekR = Nothing
  rExec (ISeq (_ :|> x)) PeekR = Just x

  rExec (IHashMap m) (Get k) = HM.lookup k m

  rExec _ _ = undefined

instance WriteE Struct where
  wExec (ISeq Empty) (PushL x) = (ISeq $ SQ.singleton x, Nothing)
  wExec (ISeq xs) (PushL x) = (ISeq (x :<| xs), Nothing)

  wExec (ISeq Empty) (PushR x) = (ISeq $ SQ.singleton x, Nothing)
  wExec (ISeq xs) (PushR x) = (ISeq (xs :|> x), Nothing)

  wExec s@(ISeq Empty) PopL = (s, Nothing)
  wExec (ISeq (x :<| xs)) PopL = (ISeq xs, Just x)

  wExec s@(ISeq Empty) PopR = (s, Nothing)
  wExec (ISeq (xs :|> x)) PopR = (ISeq xs, Just x)

  wExec (IHashMap m) (Put k v) = (IHashMap $ HM.insert k v m, Nothing)

  wExec _ _ = undefined


type Index = HashMap ByteString (MVar Struct)

data PState = PState
  { index :: MVar Index }


op :: Parser Op
op =
      ( (IWOp . PushL) <$> ("PUSHL " *> takeByteString) )
  <|> ( (IWOp . PushR) <$> ("PUSHR " *> takeByteString) )
  <|> ( "POPL" *> endOfInput *> return (IWOp PopL) )
  <|> ( "POPR" *> endOfInput *> return (IWOp PopR) )
  <|> ( "PEEKL" *> endOfInput *> return (IROp PeekL) )
  <|> ( "PEEKR" *> endOfInput *> return (IROp PeekR) )
  <|> ( (\x y -> IWOp (Put x y)) <$> ("PUT " *> (takeWhile1 (not . isSpace))) <*> (" " *> takeByteString) )
  <|> ( (IROp . Get) <$> ("GET " *> takeByteString) )

on :: Parser (a -> OnStruct a)
on = (On <$> ("ON " *> takeWhile1 (not . isSpace)) <* " ")

onop :: Parser (OnStruct Op)
onop = ($) <$> on <*> op


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


onRecv :: Socket -> IO (ByteString, SockAddr)
onRecv sock = recvFrom sock 1232

handle :: (ByteString, SockAddr) -> StateT PState IO ()
handle (raw, addr) = do
  lift $ print raw
  let (Right (On sn o)) = parseOnly onop raw
  state <- get
  idx <- lift $ readMVar $ index state
  let s = HM.lookup sn idx
  case s of
    Nothing -> lift $ print "not found"
    Just (js) -> (lift $ exec js o) >>= (lift . print)
  return ()

listenR :: Socket -> StateT PState IO ()
listenR sock = forever ((lift $ onRecv sock) >>= handle)

main :: IO ()
main = do
  let seq = ISeq (SQ.empty)
  seqMV <- newMVar seq
  let hm = IHashMap (HM.empty)
  hmMV <- newMVar hm

  let idx = (HM.fromList [("mySeq", seqMV), ("myMap", hmMV)]) :: Index
  idxMV <- newMVar idx

  let state = PState { index = idxMV }
  --stateMV <- newMVar state

  addr <- head <$> getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  sock <- socket (addrFamily addr) Datagram defaultProtocol
  bind sock (addrAddress addr)
  putStr "Listening on " >> (print $ addrAddress addr)
  runStateT (listenR sock) state
  return ()
