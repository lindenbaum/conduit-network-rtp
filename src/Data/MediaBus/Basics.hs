{-# LANGUAGE UndecidableInstances #-}

module Data.MediaBus.Basics
    ( HasFormat(..)
    , IdentifiedBy(..)
    , HasIdentifiedBy(..)
      --    , Event(..)
    ) where

import           Control.Lens


-- | A type class for media formats, like encodings, sample rate, etc...
class SetFormat s (GetFormat s) ~ s =>
      HasFormat s where
    type SetFormat s t
    type GetFormat s
    format :: Lens s (SetFormat s t) (GetFormat s) t

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
