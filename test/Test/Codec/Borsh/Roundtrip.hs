{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.Borsh.Roundtrip (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Codec.Borsh

import Test.Codec.Borsh.Util.RandomType

tests :: TestTree
tests = testGroup "Test.Codec.Borsh.Roundtrip" [
      testProperty "roundtrip" test_roundtrip
    ]

test_roundtrip :: SomeBorshValue -> Property
test_roundtrip (SomeValue _typ val) =
        deserialiseBorsh (serialiseBorsh val)
    === Right val
