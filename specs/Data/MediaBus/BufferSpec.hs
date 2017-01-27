module Data.MediaBus.BufferSpec ( spec ) where

import           Data.MediaBus
import           Test.Hspec
import           Control.Lens
import           Data.Char
import           Control.Monad.ST
import           Data.Vector.Storable.Mutable

spec :: Spec
spec = describe "IsBuffer" $ do
    it "can be mapped over with eachSample" $
        (MkTestBuffer "Hello" & eachSample %~ toUpper) `shouldBe`
            MkTestBuffer "HELLO"

data TestFormat = MkTestFormat
    deriving Show

newtype TestBuffer a = MkTestBuffer { _testBuffer :: [a] }
    deriving (Eq, Show)

makeLenses ''TestBuffer

instance Storable a =>
         IsBuffer (TestBuffer a) where
    type BufferElement (TestBuffer a) = a
    eachSample = testBuffer . each--    mutableVector =
