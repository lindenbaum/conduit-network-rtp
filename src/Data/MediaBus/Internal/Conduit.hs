module Data.MediaBus.Internal.Conduit
    ( overRightC
    , fmapMC
    , dbgShowC
    ) where

import           Conduit
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

dbgShowC :: (Show a, Monad m) => Double -> String -> Conduit a m a
dbgShowC probability msg =
    evalStateC (mkStdGen 100, 0 :: Integer) $
        awaitForever $
        \x -> do
            (g, omitted) <- State.get
            let (p, g') = randomR (0, 1) g
            if p < probability
                then do
                    let prefix = if omitted == 0
                                 then ""
                                 else "(" ++
                                     show omitted ++
                                     " messages omitted) "
                    traceM (prefix ++ msg ++ ": " ++ show x)
                    State.put (g', 0)
                else State.put (g', omitted + 1)
            yield x
