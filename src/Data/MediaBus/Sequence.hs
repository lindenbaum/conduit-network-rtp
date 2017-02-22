module Data.MediaBus.Sequence
    ( SeqNum(..)
    , HasSeqNumT(..)
    , HasSeqNum(..)
    , fromSeqNum
    , synchronizeToSeqNum
    , Discontinous(..)
    ) where

import           Test.QuickCheck                 ( Arbitrary(..) )
import           Conduit
import           Data.MediaBus.Internal.Monotone
import           Data.MediaBus.Internal.Series
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Default
import           Text.Printf
import           GHC.Generics                    ( Generic )
import           Control.DeepSeq
import           System.Random

class SetSeqNum t (GetSeqNum t) ~ t =>
      HasSeqNumT t where
    type GetSeqNum t
    type SetSeqNum t s

class HasSeqNumT t =>
      HasSeqNum t where
    seqNum :: Lens t (SetSeqNum t s) (GetSeqNum t) s

instance (HasSeqNumT a, HasSeqNumT b, GetSeqNum a ~ GetSeqNum b) =>
         HasSeqNumT (Series a b) where
    type GetSeqNum (Series a b) = GetSeqNum a
    type SetSeqNum (Series a b) t = Series (SetSeqNum a t) (SetSeqNum b t)

instance (HasSeqNum a, HasSeqNum b, GetSeqNum a ~ GetSeqNum b) =>
         HasSeqNum (Series a b) where
    seqNum f (Start a) = Start <$> seqNum f a
    seqNum f (Next b) = Next <$> seqNum f b

newtype SeqNum s = MkSeqNum { _fromSeqNum :: s }
    deriving (Num, Eq, Bounded, Enum, LocalOrd, Arbitrary, Default, Generic, Random)

instance NFData s =>
         NFData (SeqNum s)

makeLenses ''SeqNum

instance HasSeqNumT (SeqNum s) where
    type GetSeqNum (SeqNum s) = s
    type SetSeqNum (SeqNum s) s' = SeqNum s'

instance HasSeqNum (SeqNum s) where
    seqNum = fromSeqNum

instance Show s =>
         Show (SeqNum s) where
    show (MkSeqNum s) = printf "SEQNUM: %10s" (show s)

instance (Eq a, LocalOrd a) =>
         Ord (SeqNum a) where
    compare x y
        | x == y = EQ
        | x `succeeds` y = GT
        | otherwise = LT

deriving instance (Real a, Num a, Eq a, LocalOrd a) => Real
         (SeqNum a)

deriving instance (Integral a, Enum a, Real a, Eq a, LocalOrd a) =>
         Integral (SeqNum a)

synchronizeToSeqNum :: (HasSeqNum a, Monad m, Integral i)
                    => i
                    -> Conduit a m (SetSeqNum a i)
synchronizeToSeqNum startSeq =
    evalStateC startSeq (awaitForever yieldSeq)
  where
    yieldSeq a = do
        nextSeq <- get
        modify (+ 1)
        yield (a & seqNum .~ nextSeq)

-- -----------------------------------------------------
-- * Dealing with gaps in media streams
-- -----------------------------------------------------
-- | Differentiate between continuous and non-continous occurences of something.
data Discontinous a = Gap
                    | Continue a
    deriving (Show)

instance Functor Discontinous where
    fmap f (Continue x) = Continue (f x)
    fmap _f Gap = Gap
