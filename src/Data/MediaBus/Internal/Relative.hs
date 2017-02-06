module Data.MediaBus.Internal.Relative
    ( Series(..)
    , _Start
    , _Next
    , seriesStartValue
    , seriesValue
    , StartingFrom(..)
    , startingFromValue
    , overSeriesC
    , Relative(..)
    , _MkReference
    , _MkRelative
    , reference
    , relative
    , overRelativeC
    , firstAsReferenceC
    , overRelativeFirstC
    , Absolute(..)
    , absStart
    , absOffset
    ) where

import           Control.Lens
import           Conduit
import           Control.Monad.Reader
import           Control.Monad.State

data Series a b = Start { _seriesStartValue :: a }
                | Next { _seriesValue :: b }

makeLenses ''Series

makePrisms ''Series

instance Functor (Series a) where
  fmap = over _Next

newtype StartingFrom a = MkStartingFrom { _startingFromValue :: a }

makeLenses ''StartingFrom

overSeriesC :: Monad m
            => (b -> a)
            -> (StartingFrom a -> ConduitM b c m ())
            -> ConduitM (Series a b) (Series a c) m ()
overSeriesC fInitialStart fc =
    await >>=
        maybe (return ())
              (\firstA -> evalStateC (getInitialStart firstA)
                                     (go firstA >> awaitForever go))
  where
    getInitialStart (Start a) =
        MkStartingFrom a
    getInitialStart (Next b) =
        MkStartingFrom (fInitialStart b)
    go (Start x) = do
        put (MkStartingFrom x)
        yield (Start x)
    go (Next x) = do
        r <- get
        transPipe lift (yield x .| mapOutput Next (fc r))

instance (Show a, Show b) =>
         Show (Series a b) where
    show (Start x) = "START: " ++ show x
    show (Next x) = "NEXT: " ++ show x

data Relative a b = MkReference { _reference :: a }
                  | MkRelative { _relative :: b }

makeLenses ''Relative

makePrisms ''Relative

instance Functor (Relative a) where
  fmap = over _MkRelative

instance (Show a, Show b) =>
         Show (Relative a b) where
    show (MkReference x) = "REF: " ++ show x
    show (MkRelative x) = show x

overRelativeC :: Monad m
              => ConduitM b c (ReaderT (Maybe a) m) ()
              -> ConduitM (Relative a b) (Relative a c) m ()
overRelativeC fc = awaitForever (evalStateC Nothing . go)
  where
    go (MkReference ref) = put (Just ref)
    go (MkRelative x) = do
        mr <- get
        transPipe lift (runReaderC mr (yield x .| mapOutput MkRelative fc))

overRelativeFirstC :: Monad m
                   => (b -> a)
                   -> (a -> ConduitM b c m ())
                   -> ConduitM (Relative a b) (Relative a c) m ()
overRelativeFirstC initialRefFromRelative fc =
    awaitForever (evalStateC Nothing . go)
  where
    go (MkReference ref) = put (Just ref)
    go (MkRelative x) = do
        mr <- get
        r <- case mr of
                 Nothing -> do
                     let r = initialRefFromRelative x
                     put (Just r)
                     return r
                 Just r -> return r
        transPipe lift (yield x .| mapOutput MkRelative (fc r))

firstAsReferenceC :: Monad m => (b -> a) -> ConduitM b (Relative a b) m ()
firstAsReferenceC toRef = do
    mb <- await
    case mb of
        Nothing -> return ()
        Just b -> do
            yield (MkReference (toRef b))
            yield (MkRelative b)
            awaitForever (yield . MkRelative)

data Absolute a = MkAbsolute { _absStart  :: a
                             , _absOffset :: a
                             }

makeLenses ''Absolute
