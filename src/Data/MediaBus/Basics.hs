{-# LANGUAGE UndecidableInstances #-}

module Data.MediaBus.Basics
    ( IdentifiedBy(..)
    , HasIdentifiedBy(..)
    ) where

--    , Event(..)
import           Control.Lens

-- | Things that can be uniquely identified by a looking at a (much simpler)
-- representation, the 'identity'.
data IdentifiedBy i c = MkIdentifiedBy { identifier   :: i
                                       , unIdentified :: c
                                       }
    deriving (Show)

makeClassy ''IdentifiedBy

instance Functor (IdentifiedBy i) where
    fmap f (MkIdentifiedBy i c) =
        MkIdentifiedBy i (f c)
