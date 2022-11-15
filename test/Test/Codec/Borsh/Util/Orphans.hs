{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Codec.Borsh.Util.Orphans () where

import Control.Monad (replicateM)
import Data.SOP
import qualified Data.Text as Text
import Data.Text (Text)
import Data.WideWord.Word128
import Data.WideWord.Int128
import GHC.TypeLits
import Test.QuickCheck hiding (Fn)

import Data.FixedSizeArray (FixedSizeArray)

import qualified Data.FixedSizeArray as FSA

instance Arbitrary Word128 where
  arbitrary = Word128 <$> arbitrary <*> arbitrary
  shrink w128 = concat [
        [Word128 hi' lo  | hi' <- shrink hi]
      , [Word128 hi  lo' | lo' <- shrink lo]
      ]
    where
      hi = word128Hi64 w128
      lo = word128Lo64 w128

instance Arbitrary Int128 where
  arbitrary = Int128 <$> arbitrary <*> arbitrary
  shrink i128 = concat [
        [Int128 hi' lo  | hi' <- shrink hi]
      , [Int128 hi  lo' | lo' <- shrink lo]
      ]
    where
      lo = int128Lo64 i128
      hi = int128Hi64 i128

instance Arbitrary Text where
  arbitrary = Text.pack <$> arbitrary
  shrink    = map Text.pack . shrink . Text.unpack

instance (KnownNat n, Arbitrary a) => Arbitrary (FixedSizeArray n a) where
  arbitrary = FSA.fromList <$>
      replicateM (fromIntegral $ natVal (Proxy @n)) arbitrary
