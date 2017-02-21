module Data.MediaBus.Internal.Conduit
    ( overRightC
    , fmapMC
    , annotateTypeC
    , dbgShowC
    , dbgShowSink
    ) where

import           Conduit
import           Data.Conduit.List
import           Control.Lens
import           Control.Monad.State.Strict as State
import           Data.Functor
import           Debug.Trace
import           System.Random

overRightC :: Monad m
           => (forall l r r'. Iso (s l r) (s l r') (l, r) (l, r'))
           -> Conduit right (StateT left m) right'
           -> Conduit (s left right) m (s left right')
overRightC sumIso f = evalStateC (error "unreachable code")
                                 (beforeC .| f .| afterC)
  where
    beforeC = awaitForever go
      where
        go x = do
            put (x ^. sumIso . _1)
            yield (x ^. sumIso . _2)

    afterC = awaitForever go
      where
        go out = do
            x <- get
            yield (view (from sumIso) (x, out))

fmapMC :: (Functor f, Monad m) => (f a -> m b) -> Conduit (f a) m (f b)
fmapMC f = awaitForever go
  where
    go sn = do
        b <- lift (f sn)
        yield (sn $> b)

annotateTypeC :: proxy a -> Conduit a m a -> Conduit a m a
annotateTypeC _ = id

dbgShowC :: (Show a, Monad m) => Double -> String -> Conduit a m a
dbgShowC probability msg =
    evalStateC (mkStdGen 100, 0 :: Integer) $
        awaitForever $
        \x -> do
            (g, omitted) <- State.get
            let (p, g') = randomR (0, 1) g
            if p < probability
                then do
                    let omittedmsg = if omitted == 0
                                     then ""
                                     else " *** " ++
                                         show omitted ++
                                         " messages omitted"
                    traceM ((if null msg then "" else msg ++ ": ") ++ show x ++
                                omittedmsg)
                    State.put (g', 0)
                else State.put (g', omitted + 1)
            yield x

dbgShowSink :: (Show a, Monad m) => Double -> String -> Consumer a m [a]
dbgShowSink probability msg =
    dbgShowC probability msg .| consume
