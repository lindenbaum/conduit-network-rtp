module Data.MediaBus.Transcoder
    ( Transcoder(..)
    , transcodeStreamC'
    ) where

import           Conduit
import           Data.MediaBus.Stream
import           Data.Kind
import           Control.Parallel.Strategies ( NFData )

class Transcoder from to where
    type TranscodingM from to (m :: Type -> Type) :: Constraint
    type TranscodingM from to m = Monad m
    type TranscodingSeqNum from to s :: Constraint
    type TranscodingSeqNum from to s = ()
    type TranscodingTicks from to t :: Constraint
    type TranscodingTicks from to t = ()
    transcode :: (TranscodingM from to m, TranscodingSeqNum from to s, TranscodingTicks from to t, Monad m)
              => Frame s t from
              -> m (Frame s t to)

transcodeStreamC' :: (NFData to, NFData i, NFData s, NFData t, Monad m, Transcoder from to, TranscodingM from to m, TranscodingSeqNum from to s, TranscodingTicks from to t)
                 => Conduit (Stream i s t from) m (Stream i s t to)
transcodeStreamC' = mapFramesC' transcode
