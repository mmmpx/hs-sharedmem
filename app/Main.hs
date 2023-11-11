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
import Data.ByteString.Char8 (unpack)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Maybe

import Data.Sequence (Seq (..))
import qualified Data.Sequence as SQ

import Network.Socket
import Network.Socket.ByteString (recvFrom, sendAllTo)


data ROp =
    PeekL
  | PeekR
  | Get ByteString
  | At Int
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
    IList [ByteString]
  | ISeq (Seq ByteString)
  | IHashMap (HashMap ByteString ByteString)
  deriving Show

data OnStruct a = On ByteString a

instance ReadE Struct where
  rExec (IList xs) (At i) = case (i < length xs) of
    True -> Just $ xs !! i
    False -> Nothing
  rExec (IList []) PeekL = Nothing
  rExec (IList (x:xs)) PeekL = Just x
  rExec (IList []) PeekR = Nothing
  rExec (IList xs) PeekR = Just $ last xs

  rExec (ISeq Empty) PeekL = Nothing
  rExec (ISeq (x :<| _)) PeekL = Just x
  rExec (ISeq Empty) PeekR = Nothing
  rExec (ISeq (_ :|> x)) PeekR = Just x

  rExec (IHashMap m) (Get k) = HM.lookup k m

  rExec _ _ = undefined

instance WriteE Struct where
  wExec (IList xs) (PushL x) = (IList (x:xs), Nothing)
  wExec (IList xs) (PushR x) = (IList (xs ++ [x]), Nothing)
  wExec s@(IList []) PopL = (s, Nothing)
  wExec (IList (x:xs)) PopL = (IList xs, Just x)
  wExec s@(IList []) PopR = (s, Nothing)
  wExec (IList xs) PopR = (IList $ init xs, Just $ last xs)

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
  <|> ( (IROp . At . read . unpack) <$> ("AT " *> (takeWhile1 isDigit) <* endOfInput) )

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

handle :: Socket -> (ByteString, SockAddr) -> StateT PState IO ()
handle sock (raw, addr) = do
  lift $ print raw
  let (Right (On sn o)) = parseOnly onop raw
  state <- get
  idx <- lift $ readMVar $ index state
  let s = HM.lookup sn idx
  case s of
    Nothing -> lift $ sendAllTo sock "[NOT FOUND]" addr
    Just (js) -> lift ((exec js o) >>= ((flip (sendAllTo sock)) addr . (fromMaybe "(nil)")))
  return ()

listenR :: Socket -> StateT PState IO ()
listenR sock = forever ((lift $ onRecv sock) >>= handle sock)

main :: IO ()
main = do
  let seq = ISeq (SQ.empty)
  seqMV <- newMVar seq
  let hm = IHashMap (HM.empty)
  hmMV <- newMVar hm
  let l = IList []
  lMV <- newMVar l

  let idx = (HM.fromList [("mySeq", seqMV), ("myMap", hmMV), ("myList", lMV)]) :: Index
  idxMV <- newMVar idx

  let state = PState { index = idxMV }
  --stateMV <- newMVar state

  addr <- head <$> getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  sock <- socket (addrFamily addr) Datagram defaultProtocol
  bind sock (addrAddress addr)
  putStr "Listening on " >> (print $ addrAddress addr)
  runStateT (listenR sock) state
  return ()
