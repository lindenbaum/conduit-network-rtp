module Data.MediaBus.Encoding ( ) where


-- | Base type for content wrapped into encodings
newtype EncodedWith codec a = MkEncodedWith a
    deriving (Functor, Show)
