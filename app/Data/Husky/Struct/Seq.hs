module Data.Husky.Struct.Seq
  ( ix )
  where

import Control.Applicative
import Control.Concurrent.MVar
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Husky.Struct.Types
import Data.Sequence (Seq (..))
import qualified Data.Sequence as SQ

type ROp = Seq ByteString -> Maybe ByteString
type WOp = Seq ByteString -> (Seq ByteString, Maybe ByteString)

pushL :: ByteString -> WOp
pushL x xs = (x :<| xs, Nothing)

pushR :: ByteString -> WOp
pushR x xs = (xs :|> x, Nothing)

popL :: WOp
popL Empty = (Empty, Nothing)
popL (x :<| xs) = (xs, Just x)

popR :: WOp
popR Empty = (Empty, Nothing)
popR (xs :|> x) = (xs, Just x)

peekL :: ROp
peekL Empty = Nothing
peekL (x :<| _) = Just x

peekR :: ROp
peekR Empty = Nothing
peekR (_ :|> x) = Just x

op :: Parser (MVarOp (Seq ByteString))
op =
      ((MkW . pushL) <$> (word8 0 *> takeByteString))
  <|> ((MkW . pushR) <$> (word8 1 *> takeByteString))
  <|> (word8 2 *> return (MkW popL))
  <|> (word8 3 *> return (MkW popR))
  <|> (word8 4 *> return (MkR peekL))
  <|> (word8 5 *> return (MkR peekR))

ex :: ByteString -> MVar (Seq ByteString) -> IO (Maybe (Maybe ByteString))
ex s m = case (parseOnly op s) of
  (Right o) -> Just <$> exMVO o m
  (Left _) -> return Nothing

ix :: IO OpExec
ix = Mk ex <$> newMVar SQ.empty

