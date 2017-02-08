module Data.MediaBus.Internal.Series
    ( Series(..)
    , type Series'
    , _Start
    , _Next
    , seriesStartValue
    , seriesValue
    , StartingFrom(..)
    , startingFromValue
    , overSeriesC'
    , overSeriesC
    , monotoneSeriesC
    ) where

import           Control.Lens
import           Conduit
import           Control.Monad.Reader
import           Control.Monad.State
import           Test.QuickCheck

data Series a b = Next { _seriesValue :: b }
                | Start { _seriesStartValue :: a }
    deriving (Eq)

instance (Show a, Show b) =>
         Show (Series a b) where
    show (Start x) = "(START: " ++ show x ++ ")"
    show (Next x) = show x

instance (Ord a, Ord b) =>
         Ord (Series a b) where
    compare (Next l) (Next r) =
        compare l r
    compare _ _ = EQ

type Series' a = Series a a

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Series a b) where
    arbitrary = do
        isNext <- choose (0.0, 1.0)
        if isNext < (0.95 :: Double)
            then Next <$> arbitrary
            else Start <$> arbitrary

makeLenses ''Series

makePrisms ''Series

instance Functor (Series a) where
    fmap = over _Next

newtype StartingFrom a = MkStartingFrom { _startingFromValue :: a }
    deriving (Eq, Ord, Arbitrary)

makeLenses ''StartingFrom

instance Show a =>
         Show (StartingFrom a) where
    show (MkStartingFrom x) =
        "(STARTING-FROM: " ++ show x ++ ")"

overSeriesC' :: Monad m
             => (b -> a)
             -> (StartingFrom a -> ConduitM b c m ())
             -> Conduit (Series a b) m (Series a c)
overSeriesC' fInitialStart fc =
    evalStateC Nothing (await >>= maybe (return ()) starts)
  where
    starts (Next x) = do
        let r = fInitialStart x
        -- yield (Start r) -- TODO should we fake a 'Start' event?
        (yield x >> nexts) .| runNested r
        restart
    starts (Start r) = do
        yield (Start r)
        nexts .| runNested r
        restart
    restart = do
        mS <- get
        put Nothing
        case mS of
            Just s -> starts s
            Nothing -> return ()
    nexts = do
        mx <- await
        case mx of
            Just (Next x) -> yield x >> nexts
            o -> put o
    runNested initarg = transPipe lift
                                  (mapOutput Next (fc (MkStartingFrom initarg)))

overSeriesC :: Monad m
            => a
            -> (StartingFrom a -> Conduit b m c)
            -> Conduit (Series a b) m (Series a c)
overSeriesC initialA = overSeriesC' (const initialA)

monotoneSeriesC :: Monad m => m a -> (i -> m b) -> Conduit i m (Series a b)
monotoneSeriesC initSeries continueSeries = do
    rStart <- lift initSeries
    yield (Start rStart)
    mi <- await
    mapM_ (lift . continueSeries >=>
               yield . Next >=>
                   const (awaitForever (lift . continueSeries >=> yield . Next)))
          mi
