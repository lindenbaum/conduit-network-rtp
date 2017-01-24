module Data.MediaBus.Basics (IdentifiedBy(..), Event(..), Sample(..) ) where

import Data.Function (on)

-- -----------------------------------------------------
-- * Media Data Processing Base Types
-- -----------------------------------------------------
-- | Things that can be uniquely identified by a looking at a (much simpler)
-- representation, the 'identity'.
data IdentifiedBy i c = MkIdentifiedBy { identifier   :: i
                                       , unIdentified :: c
                                       }
    deriving (Show)

instance Functor (IdentifiedBy i) where
    fmap f (MkIdentifiedBy i c) =
        MkIdentifiedBy i (f c)

-- | Indication of a state change in a stream
data Event a b = Init a
               | Handle b
               | Terminate
    deriving (Show)

instance Functor (Event a) where
    fmap f (Handle x) = Handle (f x)
    fmap _f (Init x) = Init x
    fmap _f Terminate = Terminate

-- | A 'Sample' can be anything that has a start time and is exactly one time
-- unit long, it can respresent anything ranging from an audio buffer with 20ms
-- of audio to a single pulse coded audio sample, of course it could also be a
-- video frame or a chat message.
data Sample t s = MkSample { _presentationTime :: t
                           , _sampleData       :: s
                           }

instance (Show s, Show t) =>
         Show (Sample s t) where
    show (MkSample t s) = "<@" ++ show t ++ ": " ++ show s ++ "@>"

instance Eq i =>
         Eq (Sample i c) where
    (==) = (==) `on` _presentationTime

instance Ord i =>
         Ord (Sample i c) where
    compare = compare `on` _presentationTime

instance Functor (Sample t) where
    fmap f (MkSample t x) = MkSample t (f x)
