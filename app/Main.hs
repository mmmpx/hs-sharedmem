module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)

import qualified Data.Attoparsec.ByteString as AP

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Sequence (Seq (..))
import qualified Data.Sequence as SQ

import Network.Socket
import Network.Socket.ByteString (recvFrom)


data ROp = Peek
data WOp = Push ByteString | Pop
data Op = IROp ROp | IWOp WOp


class ReadE a where
  rExec :: a -> ROp -> Maybe ByteString

class WriteE a where
  wExec :: a -> WOp -> (a, Maybe ByteString)


data Struct = ISeq (Seq ByteString)
  deriving Show

instance ReadE Struct where
  rExec (ISeq Empty) Peek = Nothing
  rExec (ISeq (xs :|> x)) Peek = Just x

instance WriteE Struct where
  wExec s@(ISeq Empty) Pop = (s, Nothing)
  wExec s@(ISeq (xs :|> x)) Pop = (ISeq xs, Just x)
  wExec s@(ISeq Empty) (Push x) = (ISeq $ SQ.singleton x, Nothing)
  wExec s@(ISeq xs) (Push x) = (ISeq (x :<| xs), Nothing)


type Index = HashMap ByteString (MVar Struct)

data PState = PState
  { index :: MVar Index }


op :: AP.Parser Op
op =
      ((IWOp . Push) <$> (AP.string (pack "PUSH ") >> AP.takeWhile1 (\w -> w /= 10)))
  <|> (AP.string (pack "POP") >> AP.word8 10 >> return (IWOp Pop))
  <|> (AP.string (pack "PEEK") >> AP.word8 10 >> return (IROp Peek))

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
  let (Right o) = AP.parseOnly op raw
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
