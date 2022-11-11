{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Codec.Borsh.Util.Orphans () where

import Control.Monad (replicateM)
import Data.SOP
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.TypeLits
import Test.QuickCheck hiding (Fn)


import Data.FixedSizeArray (FixedSizeArray)
import Data.Word128
import Data.Int128

import qualified Data.FixedSizeArray as FSA

instance Arbitrary Word128 where
  arbitrary = word128 <$> arbitrary <*> arbitrary
  shrink w128 = concat [
        [word128 hi' lo  | hi' <- shrink hi]
      , [word128 hi  lo' | lo' <- shrink lo]
      ]
    where
      hi = word128MS64 w128
      lo = word128LS64 w128

instance Arbitrary Int128 where
  arbitrary = int128 <$> arbitrary <*> arbitrary
  shrink i128 = concat [
        [int128 hi' lo  | hi' <- shrink hi]
      , [int128 hi  lo' | lo' <- shrink lo]
      ]
    where
      hi = int128MS64 i128
      lo = int128LS64 i128

instance Arbitrary Text where
  arbitrary = Text.pack <$> arbitrary
  shrink    = map Text.pack . shrink . Text.unpack

instance (KnownNat n, Arbitrary a) => Arbitrary (FixedSizeArray n a) where
  arbitrary = FSA.fromList <$>
      replicateM (fromIntegral $ natVal (Proxy @n)) arbitrary

