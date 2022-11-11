module Test.Codec.Borsh.Size (tests) where

import Data.Proxy
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.ByteString.Lazy as L

import Codec.Borsh

import Test.Codec.Borsh.Util.RandomType

tests :: TestTree
tests = testGroup "Test.Codec.Borsh.Size" [
      testProperty "size" test_size
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


