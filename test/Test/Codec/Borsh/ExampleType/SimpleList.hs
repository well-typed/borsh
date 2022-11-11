module Test.Codec.Borsh.ExampleType.SimpleList (
    SimpleList(..)
  , arbitraryLargeSimpleList
  , arbitrarySimpleListOfSize
  ) where

import Control.Monad
import Data.Word
import Generics.SOP
import Test.QuickCheck

import Codec.Borsh

-- | Lists of 1s and 2s
newtype SimpleList = SimpleList { getSimpleList :: [Word8] }
  deriving newtype (Show, Eq, Ord, Generic, BorshSize, FromBorsh, ToBorsh)

instance Arbitrary SimpleList where
  arbitrary = sized arbitrarySimpleListOfSize

  -- We don't want to shrink the elements of the list since we want them to stay
  -- as 1s an 2s (so the 0s in the encoding are more obvious)
  shrink (SimpleList xs) = SimpleList <$> shrinkList (const []) xs

-- | Generate large list, in the hope that it crosses chunk boundaries
arbitraryLargeSimpleList :: Gen SimpleList
arbitraryLargeSimpleList = do
    len <- choose (0, 10_000)
    arbitrarySimpleListOfSize len

arbitrarySimpleListOfSize :: Int -> Gen SimpleList
arbitrarySimpleListOfSize len = SimpleList <$> replicateM len (elements [1, 2])
