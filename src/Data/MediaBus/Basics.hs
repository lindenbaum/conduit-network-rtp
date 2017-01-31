{-# LANGUAGE UndecidableInstances #-}

module Data.MediaBus.Basics
    ( IdentifiedBy(..)
    , identifier
    , unIdentified
    ) where
import           Control.Lens

-- | Things that can be uniquely identified by a looking at a (much simpler)
-- representation, the 'identity'.
data IdentifiedBy i c = MkIdentifiedBy { _identifier   :: i
                                       , _unIdentified :: c
                                       }
    deriving (Show)

makeLenses ''IdentifiedBy

instance Functor (IdentifiedBy i) where
    fmap f (MkIdentifiedBy i c) =
        MkIdentifiedBy i (f c)
