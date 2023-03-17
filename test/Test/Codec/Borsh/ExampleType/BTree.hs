module Test.Codec.Borsh.ExampleType.BTree (
    BTree(..)
  , arbitraryBTreeWithElems
  ) where

import Data.Word (Word8)
import Generics.SOP
import Test.QuickCheck

import qualified GHC.Generics as GHC

import Codec.Borsh

import Test.Codec.Borsh.Util.QuickCheck

-- | Binary trees
data BTree a = BTip | BLeaf a | BNode (BTree a) (BTree a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, GHC.Generic, Generic)
  deriving (BorshSize, FromBorsh) via AsEnum (BTree a)

-- Manual ToBorsh instance as a sort of "golden" test for the derived FromBorsh
-- instance
instance ToBorsh a => ToBorsh (BTree a) where
  encodeBorsh = Encoder $ \case
      BTip      -> runEncoder encodeBorsh (0 :: Word8)
      BLeaf x   -> runEncoder encodeBorsh (1 :: Word8) <> runEncoder encodeBorsh x
      BNode l r ->
           runEncoder encodeBorsh (2 :: Word8)
        <> runEncoder encodeBorsh l
        <> runEncoder encodeBorsh r

arbitraryBTreeWithElems :: [a] -> Gen (BTree a)
arbitraryBTreeWithElems xs = do
    (left, right) <- split2 xs
    case (left, right) of
      ([]  , [] ) -> return $ BTip
      ([x] , [] ) -> return $ BLeaf x
      ([]  , [y]) -> return $ BLeaf y
      (l   , r  ) -> BNode <$> arbitraryBTreeWithElems l
                           <*> arbitraryBTreeWithElems r

instance Arbitrary a => Arbitrary (BTree a) where
  arbitrary :: Gen (BTree a)
  arbitrary = sized $ \sz ->
      if sz <= 0 then
        leaf
      else
        resize (sz `div` 2) $ oneof [leaf, node]
    where
      leaf = BLeaf <$> arbitrary
      node = BNode <$> arbitrary <*> arbitrary

  shrink :: BTree a -> [BTree a]
  shrink BTip        = []
  shrink (BLeaf x)   = concat [
      -- Shrink to a tip
      [ BTip ]

      -- Shrink the element
    , BLeaf <$> shrink x
    ]
  shrink (BNode l r) = concat [
      -- Shrink to the left
      [ l ]

      -- Shrink to the right
    , [ r ]

      -- Shrink the left
    , BNode <$> shrink l <*> pure r

      -- Shrink the right
    , BNode <$> pure l <*> shrink r
    ]

