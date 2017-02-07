module Data.MediaBus.Internal.AsyncConduit ( ) where

-- every 200ms poll of audio from a ringbuffer or sorted set and push them into an encoder

-- Abstract:
-- periodically(every @p@ ticks) @poll@
-- poll p inputQueue = forever (sleep p >> peek inputQueue >>= handlePollResult)
--   where handlePollResult inputFrame = yield (overwriteTimestamps (fromMaybe (fillGap p) inputFrame))
