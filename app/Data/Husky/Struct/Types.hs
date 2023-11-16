{-# LANGUAGE ExistentialQuantification #-}

module Data.Husky.Struct.Types
  ( OpExec (..)
  , MVarOp (..)
  , exMVO )
  where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)

data OpExec =
  forall a. Mk (ByteString -> a -> IO (Maybe (Maybe ByteString))) a

data MVarOp c =
    MkR (c -> Maybe ByteString)
  | MkW (c -> (c, Maybe ByteString))

exMVO :: MVarOp c -> MVar c -> IO (Maybe ByteString)
exMVO (MkR f) m = f <$> readMVar m
exMVO (MkW f) m = do
    (x2, r) <- f <$> takeMVar m
    putMVar m x2 >> return r

