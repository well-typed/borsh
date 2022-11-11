module Test.Codec.Borsh.ExampleType.SimpleStructs (
    PolyStruct(..)
  , SimpleStruct1(..)
  , SimpleStruct2(..)
  ) where

import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word64, Word16)
import Generics.SOP (Generic)
import Test.QuickCheck

import qualified GHC.Generics as GHC

import Codec.Borsh

{-------------------------------------------------------------------------------
  Polymorphic
-------------------------------------------------------------------------------}

data PolyStruct a = Poly a a a
  deriving (Show, Eq, Ord, GHC.Generic, Generic)
  deriving (BorshSize, ToBorsh, FromBorsh) via Struct (PolyStruct a)

instance Arbitrary a => Arbitrary (PolyStruct a) where
  arbitrary = Poly <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (Poly x y z) = Poly <$> shrink x <*> shrink y <*> shrink z

{-------------------------------------------------------------------------------
  Monomorphic
-------------------------------------------------------------------------------}

data SimpleStruct1 = Struct1 Word8 () Word64
  deriving (Show, Eq, Ord, GHC.Generic, Generic)
  deriving (BorshSize, ToBorsh, FromBorsh) via Struct SimpleStruct1

instance Arbitrary SimpleStruct1 where
  arbitrary = Struct1 <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (Struct1 x y z) = Struct1 <$> shrink x <*> shrink y <*> shrink z

{-------------------------------------------------------------------------------
  Monomorphic with hand-written BorshSize instance

  The hand-written 'BorshSize' instance is useful to verify that the generic
  encoder/decoder generate values of the expected size. (Conversely, the
  automatically derived instance for 'SimpleStruct1' is useful to check that the
  generic machinery for 'BorshSize' works.)
-------------------------------------------------------------------------------}

data SimpleStruct2 = Struct2 () SimpleStruct1 Word16
  deriving (Show, Eq, Ord, GHC.Generic, Generic)
  deriving (ToBorsh, FromBorsh) via Struct SimpleStruct2

instance BorshSize SimpleStruct2 where
  type StaticBorshSize SimpleStruct2 = 'HasKnownSize

  borshSize _ =
      case borshSize (Proxy @SimpleStruct1) of
        SizeKnown  n -> SizeKnown $ n + 2

instance Arbitrary SimpleStruct2 where
  arbitrary = Struct2 <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (Struct2 x y z) = Struct2 <$> shrink x <*> shrink y <*> shrink z

