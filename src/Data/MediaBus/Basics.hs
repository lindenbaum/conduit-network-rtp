{-# LANGUAGE UndecidableInstances #-}

module Data.MediaBus.Basics
    ( IdentifiedBy(..)
    , IdString
    , IdU64
    , IdU32
    , identifier
    , unIdentified
    ) where

import           Control.Lens
import           Data.Word
import           Data.Function

-- | Things that can be uniquely identified by a looking at a (much simpler)
-- representation, the 'identity'.
data IdentifiedBy i c = MkIdentifiedBy { _identifier   :: i
                                       , _unIdentified :: c
                                       }
    deriving (Show)

makeLenses ''IdentifiedBy

type IdString = IdentifiedBy String

type IdU32 = IdentifiedBy Word32

type IdU64 = IdentifiedBy Word64

instance Eq i =>
         Eq (IdentifiedBy i c) where
    (==) = (==) `on` _identifier

instance Functor (IdentifiedBy i) where
    fmap = over unIdentified
