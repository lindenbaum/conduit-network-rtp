{-# LANGUAGE BangPatterns #-}
module Data.Conduit.Audio.Partial
  ( Part()
  , parts
  , awaitPart
  , leftoverPart)
where

import           Data.Conduit

data Part a = Part !Int !a

parts :: Monad m => ConduitM a (Part a) m ()
parts = awaitForever (yield . Part 0)

awaitPart :: Monad m => ConduitM (Part a) b m (Maybe (Int, a))
awaitPart = fmap (\ (Part !o !x) -> (o, x)) <$> await

leftoverPart :: a -> Int -> ConduitM (Part a) o m ()
leftoverPart !x !startReference =
  leftover (Part startReference x)
