module Test.Codec.Borsh.Size (tests) where

import Data.Proxy
import Data.SOP.Dict
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.ByteString.Lazy as L

import Codec.Borsh

import Test.Codec.Borsh.Util.RandomType
import Test.Codec.Borsh.Util.QuickCheck

tests :: TestTree
tests = testGroup "Test.Codec.Borsh.Size" [
      testProperty "size"    test_size
    , testProperty "maxSize" test_maxSize
    ]

test_size :: SomeBorshValue -> Property
test_size =
    \(SomeValue _typ val) -> aux val
  where
    aux :: forall a. ToBorsh a => a -> Property
    aux val =
        case borshSize (Proxy @a) of
          SizeVariable -> label "Trivial" $ True
          SizeKnown n  -> L.length (serialiseBorsh val) === fromIntegral n


test_maxSize :: SomeBorshValue -> Property
test_maxSize =
    \(SomeValue typ val) -> aux typ val
  where
    aux :: forall a. ToBorsh a => BorshType a -> a -> Property
    aux typ val =
        case borshTypeMaxSize typ of
          Nothing   -> label "Trivial" $ True
          Just Dict ->
                       L.length (serialiseBorsh val)
            `assertLE` fromIntegral (borshMaxSize (Proxy @a))


