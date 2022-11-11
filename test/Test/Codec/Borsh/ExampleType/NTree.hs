module Test.Codec.Borsh.ExampleType.NTree (
    NTree(..)
  , arbitraryNTreeWithElems
  ) where

import Data.Word (Word8)
import Generics.SOP
import Test.QuickCheck

import qualified GHC.Generics as GHC

import Codec.Borsh

import Test.Codec.Borsh.Util.QuickCheck

-- | N-ary trees
data NTree a = NLeaf | NNode a [NTree a]
  deriving (
      Show
    , Eq
    , Ord
    , Functor
    , Foldable
    , Traversable
    , GHC.Generic
    , Generic
    , BorshSize
    , ToBorsh
    )
 -- Manual FromBorsh instance as a sort of "golden" test for the derived ToBorsh
-- instance
instance FromBorsh a => FromBorsh (NTree a) where
  decodeBorsh = do
      c <- decodeBorsh @Word8
      if c == 0 then
        return NLeaf
      else
        NNode <$> decodeBorsh <*> decodeBorsh

arbitraryNTreeWithElems :: [a] -> Gen (NTree a)
arbitraryNTreeWithElems []     = pure NLeaf
arbitraryNTreeWithElems (x:xs) = do
    n  <- choose (0,10)
    cs <- splitN n xs
    NNode x <$> mapM arbitraryNTreeWithElems cs

instance Arbitrary a => Arbitrary (NTree a) where
  arbitrary :: Gen (NTree a)
  arbitrary = sized $ \sz ->
      if sz <= 0 then
        leaf
      else oneof [
          resize (sz - 1) leaf
        , do
            len <- choose (0,sz)
            resize (sz `div` (len + 1)) node
        ]
    where
      leaf = pure NLeaf
      node = NNode <$> arbitrary <*> arbitrary

  shrink :: NTree a -> [NTree a]
  shrink NLeaf        = []
  shrink (NNode x cs) = concat [
      -- Shrink to a leaf
      [ NLeaf ]

      -- Shrink the children
    , NNode x <$> shrink cs

      -- Shrink the element
    , NNode <$> shrink x <*> pure cs
    ]

