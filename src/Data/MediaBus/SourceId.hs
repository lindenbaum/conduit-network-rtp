module Data.MediaBus.SourceId
    ( HasSourceIdT(..)
    , SourceId(..)
    , sourceId
    ) where

import           Control.Lens
import           Test.QuickCheck
import           Data.Default


class SetSourceId a (GetSourceId a) ~ a =>
      HasSourceIdT a where
    type GetSourceId a
    type SetSourceId a b

-- | Things that can be uniquely identified by a looking at a (much simpler)
-- representation, the 'identity'.
newtype SourceId i = MkSourceId { _sourceId :: i }
    deriving (Eq, Arbitrary, Default, Ord)

makeLenses ''SourceId

instance Show i =>
         Show (SourceId i) where
    show (MkSourceId x) = "SOURCE-ID: " ++ show x
